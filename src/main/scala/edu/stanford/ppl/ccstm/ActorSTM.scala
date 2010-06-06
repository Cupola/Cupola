package edu.stanford.ppl.ccstm

import impl.ThreadContext
import edu.stanford.ppl.ccstm.STM.AlternativeResult
import annotation.tailrec
import util.control.ControlThrowable
import actors.{Actor, InputChannel}
import edu.stanford.ppl.ccstm.Txn.{Status, RollbackCause}

object ActorSTM {
   private val saved = new ThreadLocal[ Paused ]

   def atomic[Z](block: Txn => Z)(implicit mt: MaybeTxn): Z = {
      // TODO: without partial rollback we can't properly implement failure atomicity (see issue #4)

      mt match {
         case txn: Txn => error( "ActorSTM.atomic cannot be nested!" )
         case _ => {
            // dynamic scoping for nesting
            val ctx = ThreadContext.get
            if (null != ctx.txn) {
               error( "ActorSTM.atomic cannot be nested!" )
            } else {
               topLevelAtomic(block, ctx, Nil)
            }
         }
      }
   }

   @tailrec
   private def topLevelAtomic[Z](block: Txn => Z, ctx: ThreadContext, hist: List[Txn.RollbackCause]): Z = {
      if (!ctx.alternatives.isEmpty) {
         error( "Alternatives not supported ")
      }

      // new transaction
      val txn = new Txn(hist, ctx)
      val z = attemptImpl( txn, block, new Paused( block, txn, hist ))
      if (txn.status eq Txn.Committed) {
         z
      } else {
         val cause = txn.status.rollbackCause
         cause match {
            case x: Txn.ExplicitRetryCause => Txn.awaitRetryAndDestroy(x)
            case _ => {}
         }
         // retry
         topLevelAtomic(block, ctx, cause :: hist)
      }
   }

   private def attemptImpl[ Z ]( txn: Txn, block: Txn => Z, savedVal: Paused ) : Z = {
      var nonLocalReturn: Throwable = null
      var result: Z = null.asInstanceOf[Z]
      saved.set( savedVal )
      try {
         result = block(txn)
      }
      catch {
         case RollbackError => {}
         case x: SuspendActorControl /* if( !saved.get().consumed )*/ => throw x.getCause()  // this is the addition
         case x: ControlThrowable => nonLocalReturn = x
         case x => txn.forceRollback(Txn.UserExceptionCause(x))
      }
      finally {
         saved.set( null )
      }
      txn.commitAndRethrow()
      if (null != nonLocalReturn && txn.status == Txn.Committed) throw nonLocalReturn
      result
   }

   def pause : Paused = {
      val res = saved.get()
      if( res == null ) error( "Out of context" )
      res.txn.detach
      res
   }

   def react( handler: PartialFunction[Any, Unit]): Nothing = {
      try {
         Actor.self.react( handler )
      } catch {
         case e: ControlThrowable => throw new SuspendActorControl( e )
      }
   }

   def reactWithin( msec: Long )( handler: PartialFunction[ Any, Unit ]): Nothing = {
      try {
         Actor.self.reactWithin( msec )( handler )
      } catch {
         case e: ControlThrowable => throw new SuspendActorControl( e )
      }
   }

   private class SuspendActorControl( cause: Throwable ) extends RuntimeException( cause )

   final class Paused private[ActorSTM]( topLevelClosure: Txn => _,
                                              private[ActorSTM] val txn: Txn, hist: List[ Txn.RollbackCause ]) {
      private var consumed = false
      private val sync = new AnyRef

      private def reattach = {
         sync.synchronized {
            if( consumed ) error( "Already consumed" )
            val ctx = ThreadContext.get  // correct?
            txn.attach
            consumed = true
            ctx
         }
      }

      def resume[ Z ]( block: Txn => Z ) : Unit = {
         if( txn.status eq Txn.Active ) { // only if it wasn't rolled back before
            val ctx = reattach
            attemptImpl( txn, block, this )
            if( !(txn.status eq Txn.Committed) ) {
               val cause = txn.status.rollbackCause
               cause match {
                  case x: Txn.ExplicitRetryCause => Txn.awaitRetryAndDestroy(x)
                  case _ => {}
               }
               // retry
               topLevelAtomic(block, ctx, cause :: hist)
            }
         } else {
println( "....rollback during react" )
//            val cause = txn.status.rollbackCause
//            val ctx = ThreadContext.get
////            txn.attach( ctx )
////            topLevelAtomic(topLevelClosure, ctx, cause :: hist)
//            topLevelAtomic(topLevelClosure, ctx, Nil )
         }
      }

      def forceRollback( cause: RollbackCause ) {
         reattach
         txn.forceRollback( cause )
      }

      def commit() = {
         reattach
         txn.commit()
      }
   }
}
package edu.stanford.ppl.ccstm

import impl.ThreadContext
import edu.stanford.ppl.ccstm.STM.AlternativeResult
import annotation.tailrec
import util.control.ControlThrowable
import actors.{Actor, InputChannel}

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
      val z = attemptImpl( txn, block, new Paused( txn, hist ))
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
         case x: SuspendActorControl => throw x.getCause()  // this is the addition
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

   private class SuspendActorControl( cause: Throwable ) extends RuntimeException( cause )

   final class Paused private[ActorSTM]( val txn: Txn, hist: List[ Txn.RollbackCause ]) {
      def resume[ Z ]( block: Txn => Z ) : Z = {
         val ctx = ThreadContext.get  // correct?
         txn.attach
         val z = attemptImpl( txn, block, this )
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
   }
}
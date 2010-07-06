/*
 *  RichObject.scala
 *  (ScalaCollider-Proc)
 *
 *  Copyright (c) 2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth.proc

import de.sciss.synth._
import collection.breakOut
import collection.immutable.{ Queue => IQueue }
import ProcTxn._
import de.sciss.scalaosc.{OSCBundle, OSCMessage}

/**
 *    @version 0.11, 06-Jul-10
 */
trait RichObject { def server: Server }

case class RichBuffer( buf: Buffer ) extends RichObject {
   val isOnline: RichState   = new RichState( this, "isOnline", false )
   val hasContent: RichState = new RichState( this, "hasContent", false )

   def server = buf.server

   def alloc( numFrames: Int, numChannels: Int = 1 )( implicit tx: ProcTxn ) {
      tx.add( buf.allocMsg( numFrames, numChannels ), Some( (RequiresChange, isOnline, true) ), false )
   }

   def cue( path: String, startFrame: Int = 0 )( implicit tx: ProcTxn ) {
      tx.add( buf.cueMsg( path, startFrame ), Some( (Always, hasContent, true) ), false, Map( isOnline -> true ))
   }
}

abstract class RichNode( val initOnline : Boolean ) extends RichObject {
   val isOnline: RichState = new RichState( this, "isOnline", initOnline )
   private val onEndFuns   = Ref( IQueue.empty[ Function1[ ProcTxn, Unit ]])

   // ---- constructor ----
   node.onEnd {
      ProcTxn.atomic { implicit tx =>
         val wasOnline  = isOnline.swap( false )
         val funs       = onEndFuns.swap( IQueue.empty )
         funs.foreach( f => try {
            f( tx )
         } catch { case e => e.printStackTrace })
      }
   }

   def onEnd( fun: ProcTxn => Unit )( implicit tx: ProcTxn ) {
      onEndFuns.transform( _ enqueue fun )
   }

   def node: Node

   def server = node.server

   // oh my god it's the funky shit
   // ; things start to compose nicely...
   def read( assoc: (RichAudioBus, String), autoRelease: Boolean = true )
           ( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val reader     = AudioBusNodeSetter.reader( name, rb, this )
      registerSetter( reader, autoRelease )
   }

   def write( assoc: (RichAudioBus, String), autoRelease: Boolean = true )
            ( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val writer     = AudioBusNodeSetter.writer( name, rb, this )
      registerSetter( writer, autoRelease )
   }
   
   def readWrite( assoc: (RichAudioBus, String), autoRelease: Boolean = true )
                ( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val rw         = AudioBusNodeSetter.readerWriter( name, rb, this )
      registerSetter( rw, autoRelease )
   }

   private def registerSetter( abns: AudioBusNodeSetter, autoRelease: Boolean )
                             ( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      abns.add
      if( autoRelease ) onEnd { tx0 => abns.remove( tx0 )}
      abns
   }

   def free( audible: Boolean = true )( implicit tx: ProcTxn ) {
      tx.add( node.freeMsg, Some( (IfChanges, isOnline, false) ), audible, Map( isOnline -> true ))
   }

   def set( audible: Boolean, pairs: ControlSetMap* )( implicit tx: ProcTxn ) {
      tx.add( node.setMsg( pairs: _* ), None, audible, Map( isOnline -> true ))
   }

   def setIfOnline( pairs: ControlSetMap* )( implicit tx: ProcTxn ) {
      // XXX eventually this should be like set with different failure resolution
      if( isOnline.get ) tx.add( node.setMsg( pairs: _* ), None, true, noErrors = true )
//      if( isOnline.get ) tx.add( OSCBundle(
//         OSCMessage( "/error", -1 ), node.setMsg( pairs: _* ), OSCMessage( "/error", -2 )), true )
   }

   def mapn( audible: Boolean, pairs: ControlKBusMap* )( implicit tx: ProcTxn ) {
      tx.add( node.mapnMsg( pairs: _* ), None, audible, Map( isOnline -> true ))
   }

   def mapan( audible: Boolean, pairs: ControlABusMap* )( implicit tx: ProcTxn ) {
      tx.add( node.mapanMsg( pairs: _* ), None, audible, Map( isOnline -> true ))
   }

   def moveToHead( audible: Boolean, group: RichGroup )( implicit tx: ProcTxn ) {
      tx.add( node.moveToHeadMsg( group.group ), None, audible, Map( isOnline -> true, group.isOnline -> true ))
   }

   def moveToHeadIfOnline( group: RichGroup )( implicit tx: ProcTxn ) {
      if( isOnline.get ) tx.add( node.moveToHeadMsg( group.group ), None, true, Map( group.isOnline -> true ), true )
   }

   def moveToTail( audible: Boolean, group: RichGroup )( implicit tx: ProcTxn ) {
      tx.add( node.moveToTailMsg( group.group ), None, audible, Map( isOnline -> true, group.isOnline -> true ))
   }

   def moveBefore( audible: Boolean, target: RichNode )( implicit tx: ProcTxn ) {
      tx.add( node.moveBeforeMsg( target.node ), None, audible, Map( isOnline -> true, target.isOnline -> true ))
   }

   def moveAfter( audible: Boolean, target: RichNode )( implicit tx: ProcTxn ) {
      tx.add( node.moveAfterMsg( target.node ), None, audible, Map( isOnline -> true, target.isOnline -> true ))
   }
}

case class RichSynth( synth: Synth, synthDef: RichSynthDef ) extends RichNode( false ) {
   def node: Node = synth

   def play( target: RichNode, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead,
             bufs: Seq[ RichBuffer ] = Nil )( implicit tx: ProcTxn ) {

      require( target.server == server )
      bufs.foreach( b => require( b.server == server ))

      val deps: Map[ RichState, Boolean ] = bufs.map( _.hasContent -> true )( breakOut )      
      tx.add( synth.newMsg( synthDef.name, target.node, args, addAction ), Some( (RequiresChange, isOnline, true) ),
              true, deps ++ Map( target.isOnline -> true, synthDef.isOnline -> true ))
   }
}

object RichGroup {
   def apply( group: Group ) : RichGroup = new RichGroup( group, false )
   def default( server: Server ) : RichGroup = new RichGroup( server.defaultGroup, true ) // not very fortunate XXX
}

/**
 *    @todo needs unapply and equals?
 */
class RichGroup private( val group: Group, initOnline: Boolean ) extends RichNode( initOnline ) {
   def node: Node = group

   override def toString = "RichGroup(" + group.toString + ")"

   def play( target: RichNode, addAction: AddAction = addToHead )( implicit tx: ProcTxn ) {
      require( target.server == server )

      tx.add( group.newMsg( target.node, addAction ), Some( (RequiresChange, isOnline, true) ), false,
              Map( target.isOnline -> true ))
   }
}

object RichSynthDef {
   def apply( server: Server, graph: SynthGraph )( implicit tx: ProcTxn ) : RichSynthDef =
      ProcDemiurg.getSynthDef( server, graph )
}

case class RichSynthDef( server: Server, synthDef: SynthDef ) extends RichObject {
   val isOnline: RichState = new RichState( this, "isOnline", false )

   def name : String = synthDef.name

   /**
    *    Actually checks if the def is already online.
    *    Only if that is not the case, the receive message
    *    will be queued.
    */
   def recv( implicit tx: ProcTxn ) {
      tx.add( synthDef.recvMsg, Some( (IfChanges, isOnline, true) ), false )
   }

   def play( target: RichNode, args: Seq[ ControlSetMap ] = Nil,
             addAction: AddAction = addToHead, bufs: Seq[ RichBuffer ] = Nil )( implicit tx: ProcTxn ) : RichSynth = {
      recv  // make sure it is online
      val synth   = Synth( server )
      val rs      = RichSynth( synth, this )
      rs.play( target, args, addAction, bufs )
      rs
   }
}

class RichState( obj: AnyRef, name: String, init: Boolean ) {
   private val value = Ref( init )
//   def isSatisfied( value: Boolean )( implicit tx: ProcTxn ) : Boolean = this.value() == value
//   def currentState( implicit tx: ProcTxn ) : AnyRef
   def swap( newValue: Boolean )( implicit tx: ProcTxn ) : Boolean = value.swap( newValue )
   def get( implicit tx: ProcTxn ) : Boolean = value.apply
   def set( newValue: Boolean )( implicit tx: ProcTxn ) : Unit = value.set( newValue )

   override def toString = "<" + obj.toString + " " + name + ">"
}
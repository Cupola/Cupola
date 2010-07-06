/*
 *  ProcImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.synth.proc._
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Set => ISet }
import de.sciss.synth.{ audio => arate, control => krate, _ }
import ugen.Line

/**
 *    @version 0.11, 04-Jul-10
 */
class ProcImpl( fact: FactoryImpl, val server: Server, val name: String )
extends Proc {
   proc =>

   import Proc._

   private val runningRef        = Ref[ Option[ ProcRunning ]]( None )
//   private val groupVar          = Ref[ Option[ RichGroup ]]( None )
//   private val playGroupVar      = Ref[ Option[ RichGroup ]]( None )
   private val groupsRef         = Ref[ Option[ AllGroups ]]( None )
   private val pStringValues     = Ref( Map.empty[ ProcParamString, String ])

   lazy val audioInputs          = fact.pAudioIns.map(  p => new AudioInputImpl(  this, p.name ))
   lazy val audioOutputs         = fact.pAudioOuts.map( p => new AudioOutputImpl( this, p.name ))
   lazy val controls             = fact.paramSeq.collect {
      case pControl: ProcParamControl  => new ControlImpl( proc, pControl, krate )
      case pAudio: ProcParamAudio      => new ControlImpl( proc, pAudio, arate )
   }
   private lazy val controlMap: Map[ String, ProcControl ] = controls.map( c => (c.name -> c) )( breakOut )

   def audioInput( name: String ) : ProcAudioInput    = audioInputs.find(  _.name == name ).get
   def audioOutput( name: String ) : ProcAudioOutput  = audioOutputs.find( _.name == name ).get

   def param( name: String ) : ProcParam = fact.paramMap( name )
   def params : IIdxSeq[ ProcParam ] = fact.paramSeq

   def control( name: String ) : ProcControl = controlMap( name )

   // XXX should have the edgeMap directly here instead of spread
   // across the buses!!
   def outEdges( implicit tx: ProcTxn ) : ISet[ ProcEdge ] = {
      audioOutputs.flatMap( _.edges )( breakOut )
   }

   def setString( name: String, value: String )( implicit tx: ProcTxn ) : Proc = {
      val p = fact.paramMap( name ).asInstanceOf[ ProcParamString ]
      pStringValues.transform( _ + (p -> value) )
      runningRef().foreach( _.setString( name, value ))
      this
   }

    def getString( name: String )( implicit tx: ProcTxn ) : String = {
       val p = fact.paramMap( name ).asInstanceOf[ ProcParamString ]
//          pStringValues().get( p ).getOrElse( p.default.getOrElse( pError( name )))
       pStringValues().get( p ).getOrElse( pError( name ))
   }

   def runningGroup( implicit tx: ProcTxn ) : RichGroup =
      groupsRef().map( all => all.front.map( _.core ).getOrElse( all.main )).getOrElse( RichGroup.default( server ))


   def sendToBack( xfade: XFade )( implicit tx: ProcTxn ) {
      if( !isPlaying ) return
      createBackground( xfade )
      // now re-play
      stop
      play
   }

   private def createBackground( xfade: XFade )( implicit tx: ProcTxn ) {
      if( !xfade.markSendToBack( this )) return

      val g       = group           // ensures that main group exists,
      val all     = groupsRef().get // i.e. that this is valid
      val bgOldO  = all.back
      val bgNew   = RichGroup( Group( server ))
      bgNew.play( g, addToHead )
      all.front map { f =>
         f.core.moveToTail( true, bgNew )
         f.pre.foreach( _.moveBefore( true, f.core ))
         f.post.foreach( _.moveAfter( true, f.core ))
      } getOrElse {
         runningRef().foreach( _.setGroup( bgNew ))
      }
      bgOldO.foreach( _.moveToHead( true, bgNew ))

      // XXX graphs like this could be lazy vals
      // in the companion object
      val rsd     = RichSynthDef( server, SynthGraph {
         Line.kr( dur = "$dur".ir, doneAction = freeGroup )
      })
      val rs      = rsd.play( bgNew, List( "$dur" -> xfade.dur ))
//      audioInputs.foreach(  _.sendToBack( xfade, bgNew ))
//      audioOutputs.foreach( _.sendToBack( xfade, bgNew ))
//      // XXX missing: mapped controls

      // update groups
      groupsRef.set( Some( all.copy( front = None, back = Some( bgNew ))))
   }

   private def pError( name: String ) = throw new ProcParamUnspecifiedException( name )

   def groupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupsRef().map( _.main )

   def group( implicit tx: ProcTxn ) : RichGroup = {
      groupOption getOrElse {
         val g    = Group( server )
         val res  = RichGroup( g )
         res.play( RichGroup.default( server ))
         group    = res
         res
      }
   }

   // callers should know what they are doing... XXX
   def backGroup( implicit tx: ProcTxn ) : RichGroup = groupsRef().get.back.get

   def group_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
      groupsRef.transform( _ map { all =>
         all.front.foreach( f => {
            f.core.moveToTail( true, newGroup )
            f.pre.foreach(  _.moveBefore( true, f.core ))
            f.post.foreach( _.moveAfter(  true, f.core ))
         })
         all.back.foreach( _.moveToHead( true, newGroup ))
         all.main.free( true ) // que se puede...?
         all.copy( main = newGroup )
      } orElse {
         val all = AllGroups( newGroup, None, None )
         runningRef().foreach( _.setGroup( newGroup ))
         Some( all )
      })
   }

   private def coreGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] =
      groupsRef().flatMap( _.front.map( _.core ))

   def coreGroup( implicit tx: ProcTxn ) : RichGroup = {
      coreGroupOption getOrElse {
         val g       = Group( server )
         val res     = RichGroup( g )
         res.play( group ) // creates group if necessary
         coreGroup   = res
         res
      }
   }

   private def coreGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
      groupsRef transform { allO =>
         val all = allO.get
         require( all.front.isEmpty )
         Some( all.copy( front = Some( FrontGroups( newGroup, None, None ))))
      }
      runningRef().foreach( _.setGroup( newGroup ))
   }

   private def preGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] =
      groupsRef().flatMap( _.front.flatMap( _.pre ))

   def preGroup( implicit tx: ProcTxn ) : RichGroup = {
      preGroupOption getOrElse {
         val g       = Group( server )
         val res     = RichGroup( g )
         res.play( coreGroup, addBefore ) // creates coreGroup if necessary
         preGroup    = res
         res
      }
   }

   private def preGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
      groupsRef transform { allO =>
         val all     = allO.get
         val front   = all.front.get
         require( front.pre.isEmpty )
         Some( all.copy( front = Some( front.copy( pre = Some( newGroup )))))
      }
   }

   private def postGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] =
      groupsRef().flatMap( _.front.flatMap( _.post ))

   def postGroup( implicit tx: ProcTxn ) : RichGroup = {
      postGroupOption getOrElse {
         val g       = Group( server )
         val res     = RichGroup( g )
         res.play( coreGroup, addAfter ) // creates coreGroup if necessary
         postGroup   = res
         res
      }
   }

   private def postGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
      groupsRef transform { allO =>
         val all     = allO.get
         val front   = all.front.get
         require( front.post.isEmpty )
         Some( all.copy( front = Some( front.copy( post = Some( newGroup )))))
      }
   }

//   private def backGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] =
//      groupsRef().flatMap( _.back )
//
//   private def backGroup( implicit tx: ProcTxn ) : RichGroup = {
//      backGroupOption getOrElse {
//         val g       = Group( server )
//         val res     = RichGroup( g )
//         res.play( group, addToHead ) // creates group if necessary
//         backGroup   = res
//         res
//      }
//   }
//
//   private def backGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
//      groupsRef transform { allO =>
//         val all = allO.get
//         require( all.back.isEmpty )
//         Some( all.copy( back = Some( newGroup )))
//      }
//   }

   private def backGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
      groupsRef transform { allO =>
         val all = allO.get
         require( all.back.isEmpty )
         Some( all.copy( back = Some( newGroup )))
      }
   }

   private[proc] def disconnect( out: ProcAudioOutput, in: ProcAudioInput ) {
      error( "NOT YET IMPLEMENTED" )
   }

   private[proc] def insert( out: ProcAudioOutput, in: ProcAudioInput,
                             insert: (ProcAudioInput, ProcAudioOutput) ) {
      error( "NOT YET IMPLEMENTED" )
   }

   def play( implicit tx: ProcTxn ) {
      if( isPlaying ) {
         println( "WARNING: Proc.play - '" + this + "' already playing")
      } else {
         tx transit match {
            case xfade: XFade => coreGroup // enforce XXX ugly
            case _ =>
         }
         val run = Proc.use( proc ) {
//               val target = playGroupOption.getOrElse( groupOption.getOrElse( RichGroup.default( server )))
            fact.entry.play
         }
         val runO = Some( run )
//         lazy val l: Model.Listener = {
//            case ProcRunning.Stopped => {
//               run.removeListener( l )
//               ProcTxn.atomic { t2 => if( runningRef()( t2 ) == runO ) setRunning( None )( t2 )}
//            }
//            case m => println( "Ooooops : " + m )
//         }
//         run.addListener( l )
         setRunning( runO )
      }
   }

   private def setRunning( run: Option[ ProcRunning ])( implicit tx: ProcTxn ) {
      touch
      val u    = update.apply
      val flag = Some( run.isDefined )
      if( u.playing != flag ) update.set( u.copy( playing = flag ))
      runningRef.set( run )
   }

   def stop( implicit tx: ProcTxn ) {
      runningRef().foreach( r => {
         tx transit match {
            case xfade: XFade => createBackground( xfade )
            case _ =>
         }
         r.stop
//         preGroupOption.foreach( _.freeAll )
         setRunning( None )
      })
   }

   def isPlaying( implicit tx: ProcTxn ) : Boolean = runningRef().isDefined

   private[proc] def controlChanged( ctrl: ProcControl, newValue: ControlValue )( implicit tx: ProcTxn ) {
      runningRef().foreach( run => {
         newValue.mapping match {
            case None      => run.setFloat( ctrl.name, newValue.current.toFloat )
            case Some( m ) => m.play 
         }
      })
      touch
      update.transform( u => u.copy( controls = u.controls + (ctrl -> newValue) ))
   }

//   private[proc] def controlMapped( ctrl: ProcControl, newValue: Option[ ProcControlMapping ])( implicit tx: ProcTxn ) {
//      touch
//      update.transform( u => u.copy( mappings = u.mappings + (ctrl -> newValue) ))
//   }

   private[proc] def audioBusConnected( e: ProcEdge )( implicit tx: ProcTxn ) {
      touch
      update.transform( u => if( u.audioBusesDisconnected.contains( e )) {
         u.copy( audioBusesDisconnected = u.audioBusesDisconnected - e )
      } else {
         u.copy( audioBusesConnected = u.audioBusesConnected + e )
      })
   }

   private[proc] def busParamChanged( bus: ProcAudioBus, abus: AudioBus )( implicit tx: ProcTxn ) {
//      if( verbose ) println( "PROC BUS PARAM CHANGED " + name + " / " + bus.name + " / " + abus + " / " + runningRef() )
      runningRef().foreach( _.busChanged( bus.name, abus ))
   }

   override def toString = "proc(" + name + ")"

   private case class FrontGroups( core: RichGroup, pre: Option[ RichGroup ], post: Option[ RichGroup ])
   private case class AllGroups( main: RichGroup, front: Option[ FrontGroups ], back: Option[ RichGroup ])
}
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
import de.sciss.synth.{audio => arate, control => krate, AudioBus, Model, Group, Server}
import actors.{Future, TIMEOUT}
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Set => ISet }

/**
 *    @version 0.11, 04-Jul-10
 */
class Impl( fact: FactoryImpl, val server: Server, val name: String )
extends Proc {
   proc =>

   import Proc._

   private val runningRef        = Ref[ Option[ ProcRunning ]]( None )
   private val groupVar          = Ref[ Option[ RichGroup ]]( None )
   private val playGroupVar      = Ref[ Option[ RichGroup ]]( None )
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

   private def pError( name: String ) = throw new ProcParamUnspecifiedException( name )

   def groupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupVar()

   def group( implicit tx: ProcTxn ) : RichGroup = {
      groupOption.getOrElse({
         val g    = Group( server )
         val res  = RichGroup( g )
         res.play( RichGroup.default( server ))
         group    = res
         res
      })
   }
   def group_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
//         groupOption = Some( newGroup )
      val newGroupO  = Some( newGroup )
      val oldGroupO  = groupVar.swap( newGroupO )
      val pgo = playGroupOption
      pgo.foreach( _.moveToHead( true, newGroup ))
      // XXX also move the mapping synths!
      if( pgo.isEmpty ) { // we are still playing in the main group, hence update that
         runningRef().foreach( _.setGroup( newGroup ))
      }
      // XXX what should we do, free it?
      oldGroupO.foreach( _.free( true ))
   }

   def playGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = playGroupVar()

   def playGroup( implicit tx: ProcTxn ) : RichGroup = {
//println( "playGroup" )
      playGroupOption.getOrElse({
         val g       = Group( server )
         val res     = RichGroup( g )
         res.play( group ) // creates group if necessary
         playGroup   = res
         res
      })
   }

   def playGroup_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
//println( "playGroup_" )
      val newGroupO  = Some( newGroup )
      val oldGroupO  = playGroupVar.swap( newGroupO )
      runningRef().foreach( _.setGroup( newGroup ))
      // XXX what should we do, free it?
      oldGroupO.foreach( rg => {
         rg.free( true ) // after running.setGroup !
      })
   }

   private def await[ A ]( timeOut: Long, fut: Future[ A ])( handler: Function1[ Option[ A ], Unit ]) : Nothing = {
      fut.inputChannel.reactWithin( timeOut ) {
         case TIMEOUT => handler( None )
         case a       => handler( Some( a.asInstanceOf[ A ]))
      }
   }

   private[proc] def disconnect( out: ProcAudioOutput, in: ProcAudioInput ) {
      error( "NOT YET IMPLEMENTED" )
   }

   private[proc] def insert( out: ProcAudioOutput, in: ProcAudioInput,
                             insert: (ProcAudioInput, ProcAudioOutput) ) {
      error( "NOT YET IMPLEMENTED" )
   }

   def play( implicit tx: ProcTxn ) : Proc = {
      if( isPlaying ) {
         println( "WARNING: Proc.play - '" + this + "' already playing")
      } else {
         val run = Proc.use( proc ) {
//               val target = playGroupOption.getOrElse( groupOption.getOrElse( RichGroup.default( server )))
            fact.entry.play
         }
         lazy val l: Model.Listener = {
            case ProcRunning.Stopped => {
               run.removeListener( l )
//                     exec( if( running == Some( run )) running = None ) // XXX propagate stop?
               ProcTxn.atomic { t2 => setRunning( None )( t2 )}
            }
            case m => println( "Ooooops : " + m )
         }
         run.addListener( l )
         setRunning( Some( run ))
      }
      this
   }

   private def setRunning( run: Option[ ProcRunning ])( implicit tx: ProcTxn ) {
      touch
      val u    = update.apply
      val flag = Some( run.isDefined )
      if( u.playing != flag ) update.set( u.copy( playing = flag ))
      runningRef.set( run )
   }

   def stop( implicit tx: ProcTxn ) : Proc = {
      runningRef().foreach( r => {
         try { r.stop } catch { case ex => ex.printStackTrace() }
         setRunning( None )
      })
      this
   }

   def isPlaying( implicit tx: ProcTxn ) : Boolean = runningRef().isDefined

   private[proc] def controlChanged( ctrl: ProcControl, newValue: Float )( implicit tx: ProcTxn ) {
      if( !ctrl.isMapped ) runningRef().foreach( _.setFloat( ctrl.name, newValue ))
      touch
      update.transform( u => u.copy( controls = u.controls + (ctrl -> newValue) ))
   }

   private[proc] def controlMapped( ctrl: ProcControl, newValue: Option[ ProcControlMapping ])( implicit tx: ProcTxn ) {
      touch
      update.transform( u => u.copy( mappings = u.mappings + (ctrl -> newValue) ))
   }

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
}

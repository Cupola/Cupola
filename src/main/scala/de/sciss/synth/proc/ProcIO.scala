/*
 *  ProcIO.scala
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

import de.sciss.synth.{ Bus, Rate }
import collection.immutable.{ Set => ISet }

/**
 *    @version 0.12, 02-Jul-10
 */
sealed trait ProcAudioBus {
   def proc : Proc
   def bus( implicit tx: ProcTxn ): Option[ RichAudioBus ]
   def bus_=( newBus: Option[ RichAudioBus ])( implicit tx: ProcTxn ): Unit
   def synthetic( implicit tx: ProcTxn ): Boolean
   def synthetic_=( onOff: Boolean )( implicit tx: ProcTxn ): Unit
//   def index( implicit tx: ProcTxn ) : Int // -1 : not specified
   def name : String
//   def edges( implicit tx: ProcTxn ) : ISet[ ProcEdge ]
}

trait ProcAudioOutput extends ProcAudioBus {
   /**
    *    Tries to map the audio output to a control.
    *    Throws an error if the control is not mappable.
    *    If the control is mappable but runs at control-rate,
    *    an A->K downsampler is automatically inserted.
    *
    *    @return  the target's proc, so that ~> can be chained
    *             using the implicit conversion from Proc to ProcAudioOutput
    */
   def ~>  ( control: ProcControl )( implicit tx: ProcTxn ) : Proc // = ~>( control.audioMap )
   def ~>  ( in: ProcAudioInput )( implicit tx: ProcTxn ) : Proc
   def ~/> ( in: ProcAudioInput )( implicit tx: ProcTxn ) : ProcAudioOutput
   def ~|  ( insert: (ProcAudioInput, ProcAudioOutput) )( implicit tx: ProcTxn ) : ProcAudioInsertion
}

trait ProcAudioInput extends ProcAudioBus {
   private[proc] def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) : Unit
}

trait ProcAudioInsertion {
   def |> ( in: ProcAudioInput ) : ProcAudioInput
}

trait ProcControl {
   def proc : Proc
   def name : String
   def rate : Option[ Rate ]
   def spec : ParamSpec
   def default : Float
//   def mapped( implicit tx: ProcTxn ): Option[ RichBus ]
   def value( implicit tx: ProcTxn ) : Float
   def value_=( newValue: Float )( implicit tx: ProcTxn ) : Unit

   def canMap( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : Boolean
   def map( aout: ProcAudioOutput )( implicit tx: ProcTxn ) : ProcControlAMapping
   def isMapped( implicit tx: ProcTxn ) : Boolean = mapping.isDefined
//   def mappedInput( implicit tx: ProcTxn ) : Option[ ProcAudioInput ]

//   /**
//    *    Queries the underlying internal bus onto which the control
//    *    is currently mapped. If the control is not mapped, returns
//    *    None. Otherwise it is expected to return the underlying bus,
//    *    and if such bus does not yet exist, to hereby create it!
//    *    That is, if isMapped returns true, mappedOutput is expected
//    *    to return Some( bus ).
//    */
//   private[proc] def mappedOutput( implicit tx: ProcTxn ) : Option[ RichBus ]

   def mapping( implicit tx: ProcTxn ) : Option[ ProcControlMapping ]
}

trait ProcControlMapping {
//   def outputRate
   def output( implicit tx: ProcTxn ) : RichBus
   def play( implicit tx: ProcTxn )
   def stop( implicit tx: ProcTxn )
}

trait ProcControlAMapping  // XXX not very elegant name
extends ProcControlMapping {
   def input : ProcAudioInput // ( implicit tx: ProcTxn )
}

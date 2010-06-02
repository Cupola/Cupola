//package de.sciss.synth.proc
//
//object ActorUtil {
//   def !?( timeOut: Long, p: OSCPacket, handler: PartialFunction[ Any, Unit ]) {
//       val a = new DaemonActor {
//          def act {
//             val futCh   = new Channel[ Any ]( Actor.self )
//             val oh      = new OSCTimeOutHandler( handler, futCh )
//             OSCReceiverActor.addHandler( oh )
//             server ! p // only after addHandler!
//             futCh.reactWithin( timeOut ) {
//                case TIMEOUT   => OSCReceiverActor.removeHandler( oh )
//                case r         =>
//             }
//          }
//       }
//       a.start()
//       a
//    }
//} 
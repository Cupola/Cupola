package de.sciss.cupola

import java.nio.ByteBuffer
import de.sciss.osc.{OSCPacket, OSCException, OSCPacketCodec, OSCMessage}

object OSCTrackingMessage {
   val empty = OSCTrackingMessage( 50f, 50f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ) 
}
case class OSCTrackingMessage( pointX: Float, pointY: Float, eyeLX: Int, eyeLY: Int, eyeRX: Int, eyeRY: Int,
                            pupLX: Int, pupLY: Int, pupRX: Int, pupRY: Int, blinkL: Int, blinkR: Int, state: Int )
extends OSCMessage( "/t", pointX, pointY, eyeLX, eyeLY, eyeRX, eyeRY, pupLX, pupLY, pupRX, pupRY, blinkL, blinkR, state )

object OSCTrackingCodec extends OSCPacketCodec {
   private def decodeTracking( b: ByteBuffer ) : OSCMessage = {
      // ",ffiiiii iiiiii
      if( (b.getLong() != 0x2C66666969696969L) || (b.getInt() != 0x69696969) || (b.getShort() != 0x6969) ) decodeFail
		OSCPacket.skipToValues( b )

		val pointX        = b.getFloat()
      val pointY        = b.getFloat()
      val eyeLX         = b.getInt()
      val eyeLY         = b.getInt()
      val eyeRX         = b.getInt()
      val eyeRY         = b.getInt()
      val pupLX         = b.getInt()
      val pupLY         = b.getInt()
      val pupRX         = b.getInt()
      val pupRY         = b.getInt()
      val blinkL        = b.getInt()
      val blinkR        = b.getInt()
      val state         = b.getInt()

		OSCTrackingMessage( pointX, pointY, eyeLX, eyeLY, eyeRX, eyeRY, pupLX, pupLY, pupRX, pupRY, blinkL, blinkR, state )
	}

   override protected def decodeMessage( name: String, b: ByteBuffer ) : OSCMessage = {
      if( name == "/t" ) decodeTracking( b ) else super.decodeMessage( name, b )
	}

   private def decodeFail : Nothing = throw new OSCException( OSCException.DECODE, null )
}

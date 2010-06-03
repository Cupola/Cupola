package de.sciss.synth.proc

import de.sciss.synth.Buffer

case class RichBuffer( buf: Buffer, state: RichObject.State ) extends RichObject
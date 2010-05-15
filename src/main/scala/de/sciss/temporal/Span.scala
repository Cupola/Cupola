package de.sciss.temporal

import math._

object Span {
   /**
    *  Union operation on two spans.
    *
    *  @param  span1   first span to fuse
    *  @param  span2   second span to fuse
    *  @return		a new span whose extension
    *				covers both span1 and span2
    */
   def union( span1: Span, span2: Span ) : Span = span1.unite( span2 )

   def intersection( span1: Span, span2: Span ) : Span = span1.intersect( span2 )
}

case class Span( start: Long, stop: Long ) {
   def length = stop - start

   /**
    *  Checks if a position lies within the span.
    *
    *  @return		<code>true</code>, if <code>start <= pos < stop</code>
    */
   def contains( pos: Long ) : Boolean = pos >= start && pos < stop

   /**
    *  Checks if another span lies within the span.
    *
    *	@param	aSpan	second span, may be <code>null</code> (in this case returns <code>false</code>)
    *  @return		<code>true</code>, if <code>aSpan.start >= this.span &&
    *				aSpan.stop <= this.stop</code>
    */
    def contains( aSpan: Span ) : Boolean =
         (aSpan.start >= this.start) && (aSpan.stop <= this.stop)

   /**
    *  Checks if a two spans overlap each other.
    *
    *	@param	aSpan	second span
    *  @return		<code>true</code>, if the spans
    *				overlap each other
    */
    def overlaps( aSpan: Span ) : Boolean =
      ((aSpan.start < this.stop) && (aSpan.stop > this.start)) 

   /**
    *  Checks if a two spans overlap or touch each other.
    *
    *	@param	aSpan	second span
    *  @return		<code>true</code>, if the spans
    *				overlap each other
    */
    def touches( aSpan: Span ) : Boolean =
      if( start <= aSpan.start ) {
         stop >= aSpan.start
      } else {
         aSpan.stop >= start
      }

   /**
    *  Checks if the span is empty.
    *
    *  @return		<code>true</code>, if <code>start == stop</code>
    */
   def isEmpty : Boolean = start == stop
   
   def nonEmpty : Boolean = start != stop

   def unite( aSpan: Span )      = Span( min( start, aSpan.start ), max( stop, aSpan.stop ))
   def intersect( aSpan: Span )  = Span( max( start, aSpan.start ), min( stop, aSpan.stop ))

   def clip( pos: Long ) : Long = max( start, min( stop, pos ))

   def shift( delta: Long ) = Span( start + delta, stop + delta )
}
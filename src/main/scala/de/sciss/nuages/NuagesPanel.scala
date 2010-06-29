/*
 *  NuagesPanel.scala
 *  (Wolkenpumpe)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.nuages

import java.awt.{EventQueue, Color, BorderLayout}
import javax.swing.JPanel
import collection.immutable.{ IndexedSeq => IIdxSeq, IntMap }
import prefuse.{Constants, Display, Visualization}
import prefuse.action.{RepaintAction, ActionList}
import prefuse.action.animate.{VisibilityAnimator, LocationAnimator, ColorAnimator}
import prefuse.action.layout.graph.{ForceDirectedLayout, NodeLinkTreeLayout}
import prefuse.activity.Activity
import prefuse.util.ColorLib
import prefuse.visual.sort.TreeDepthItemSorter
import prefuse.visual.expression.InGroupPredicate
import de.sciss.synth.{Model, Server}
import de.sciss.synth.swing.PrefuseHelper
import prefuse.data.{Edge, Node => PNode, Graph}
import prefuse.controls._
import prefuse.visual.{AggregateItem, VisualItem}
import de.sciss.synth.proc._
import prefuse.render._
import prefuse.action.assignment.{FontAction, ColorAction}

/**
 *    @version 0.11, 29-Jun-10
 */
object NuagesPanel {
   var verbose = false

   private[nuages] case class VisualProc( proc: Proc, pNode: PNode, aggr: AggregateItem, params: IndexedSeq[ VisualParam ]) {
      var playing = false
   }

   private[nuages] trait VisualParam {
      def param: ProcParam[ _ ]
      def pNode: PNode
      def pEdge: Edge
   }

   private[nuages] case class VisualBus( param: ProcParamAudioBus, pNode: PNode, pEdge: Edge )
   extends VisualParam

   private[nuages] case class VisualFloat( param: ProcParamFloat, pNode: PNode, pEdge: Edge )
   extends VisualParam {
      var value   = 0f 
      var mapped  = false
   }

   private[nuages] val COL_NUAGES = "nuages"
}

class NuagesPanel( server: Server ) extends JPanel {
   import NuagesPanel._
   import ProcWorld._
   import Proc._

   val vis     = new Visualization()
   val world   = ProcDemiurg.worlds( server )

   private val AGGR_PROC            = "aggr"
   private val GROUP_GRAPH          = "graph"
   private val GROUP_NODES          = "graph.nodes"
   private val GROUP_EDGES          = "graph.edges"
//   private val GROUP_PAUSED         = "paused"
   private val ACTION_LAYOUT        = "layout"
   private val ACTION_LAYOUT_ANIM   = "layout-anim"
   private val ACTION_COLOR         = "color"
   private val ACTION_COLOR_ANIM    = "layout-anim"
   private val FADE_TIME            = 333
//   private val COL_LABEL            = "name"
//   private val GROUP_PROC           = "proc"

   val g                    = {
      val res     = new Graph
      val nodes   = res.getNodeTable()
//      res.addColumn( COL_LABEL, classOf[ String ])
//val test = res.addNode()
//test.set( COL_LABEL, "HALLLLLO" )
      res
   }
   val vg   = {
      val res = vis.add( GROUP_GRAPH, g )
      res.addColumn( COL_NUAGES, classOf[ AnyRef ])
      res
   }
   private val aggrTable            = {
      val res = vis.addAggregates( AGGR_PROC )
      res.addColumn( VisualItem.POLYGON, classOf[ Array[ Float ]])
//      res.addColumn( "id", classOf[ Int ]) // XXX not needed
      res
   }
//   val procG   = {
//      vis.addFocusGroup( GROUP_PROC )
//      vis.getGroup( GROUP_PROC )
//   }
   private var procMap              = Map.empty[ Proc, VisualProc ]
   private var edgeMap              = Map.empty[ ProcEdge, Edge ]

   private val topoListener : Model.Listener = {
      case VerticesRemoved( procs @ _* )  => defer( topRemoveProcs( procs: _* ))
      case VerticesAdded( procs @ _* )    => defer( topAddProcs( procs: _* ))
      case EdgesRemoved( edges @ _* )     => defer( topRemoveEdges( edges: _* ))
      case EdgesAdded( edges @ _* )       => defer( topAddEdges( edges: _* ))
   }

   private val procListener : Model.Listener = {
      case PlayingChanged( proc, state ) => defer( topProcPlaying( proc, state ))
   }
   
   // ---- constructor ----
   {
      val font    = Wolkenpumpe.condensedFont.deriveFont( 10 )
      val display = new Display( vis )

//      vis.setValue( GROUP_NODES, null, VisualItem.SHAPE, new java.lang.Integer( Constants.SHAPE_ELLIPSE ))
//      vis.add( GROUP_GRAPH, g )
//      vis.addFocusGroup( GROUP_PAUSED, setPaused )

//      val nodeRenderer = new LabelRenderer( COL_LABEL )
      val procRenderer  = new NuagesProcRenderer( 50 )
//      val paramRenderer = new NuagesProcParamRenderer( 50 )
//      val nodeRenderer = new NuagesProcRenderer
//      nodeRenderer.setRenderType( AbstractShapeRenderer.RENDER_TYPE_FILL )
//      nodeRenderer.setHorizontalAlignment( Constants.LEFT )
//      nodeRenderer.setRoundedCorner( 8, 8 )
//      nodeRenderer.setVerticalPadding( 2 )
//      val edgeRenderer = new EdgeRenderer( Constants.EDGE_TYPE_CURVE )
      val edgeRenderer = new EdgeRenderer( Constants.EDGE_TYPE_LINE, Constants.EDGE_ARROW_FORWARD )
      val aggrRenderer = new PolygonRenderer( Constants.POLY_TYPE_CURVE )
      aggrRenderer.setCurveSlack( 0.15f )

      val rf = new DefaultRendererFactory( procRenderer )
//      val rf = new DefaultRendererFactory( new ShapeRenderer( 50 ))
//      rf.add( new InGroupPredicate( GROUP_PROC ), procRenderer )
      rf.add( new InGroupPredicate( GROUP_EDGES), edgeRenderer )
      rf.add( new InGroupPredicate( AGGR_PROC ), aggrRenderer )
//      val rf = new DefaultRendererFactory
//      rf.setDefaultRenderer( new ShapeRenderer( 20 ))
//      rf.add( "ingroup('aggregates')", aggrRenderer )
      vis.setRendererFactory( rf )

      // colors
      val actionNodeStroke = new ColorAction( GROUP_NODES, VisualItem.STROKECOLOR, ColorLib.rgb( 255, 255, 255 ))
      val actionNodeFill   = new ColorAction( GROUP_NODES, VisualItem.FILLCOLOR, ColorLib.rgb( 0, 0, 0 ))
//      actionNodeColor.add( new InGroupPredicate( GROUP_PAUSED ), ColorLib.rgb( 200, 0, 0 ))
      val actionTextColor = new ColorAction( GROUP_NODES, VisualItem.TEXTCOLOR, ColorLib.rgb( 255, 255, 255 ))

      val actionEdgeColor  = new ColorAction( GROUP_EDGES, VisualItem.STROKECOLOR, ColorLib.rgb( 255, 255, 255 ))
      val actionAggrFill   = new ColorAction( AGGR_PROC, VisualItem.FILLCOLOR, ColorLib.rgb( 127, 127, 127 ))
      val actionAggrStroke = new ColorAction( AGGR_PROC, VisualItem.STROKECOLOR, ColorLib.rgb( 255, 255, 255 ))
      val fontAction       = new FontAction( GROUP_NODES, font )

      val lay = new ForceDirectedLayout( GROUP_GRAPH )

      // quick repaint
      val actionColor = new ActionList()
      actionColor.add( fontAction )
      actionColor.add( actionTextColor )
      actionColor.add( actionNodeStroke )
      actionColor.add( actionNodeFill )
      actionColor.add( actionEdgeColor )
      actionColor.add( actionAggrFill )
      actionColor.add( actionAggrStroke )
//      actionColor.add( actionArrowColor )
      vis.putAction( ACTION_COLOR, actionColor )
      
      val actionLayout = new ActionList( Activity.INFINITY, 50 )
      actionLayout.add( lay )
      actionLayout.add( new PrefuseAggregateLayout( AGGR_PROC ))
      actionLayout.add( new RepaintAction() )
      vis.putAction( ACTION_LAYOUT, actionLayout )
//      vis.runAfter( ACTION_COLOR, ACTION_LAYOUT )
      vis.alwaysRunAfter( ACTION_COLOR, ACTION_LAYOUT )

      // ------------------------------------------------

      // initialize the display
      display.setSize( 400, 400 )
//      display.setItemSorter( new TreeDepthItemSorter() )
//      display.addControlListener( new DragControl() )
      display.addControlListener( new ZoomToFitControl() )
      display.addControlListener( new ZoomControl() )
      display.addControlListener( new WheelZoomControl() )
      display.addControlListener( new PanControl() )
      display.addControlListener( new PrefuseAggregateDragControl )
      display.setHighQuality( true )

      // ------------------------------------------------

//      nodeRenderer.setHorizontalAlignment( Constants.CENTER )
      edgeRenderer.setHorizontalAlignment1( Constants.CENTER )
      edgeRenderer.setHorizontalAlignment2( Constants.CENTER )
      edgeRenderer.setVerticalAlignment1( Constants.CENTER )
      edgeRenderer.setVerticalAlignment2( Constants.CENTER )

      display.setForeground( Color.WHITE )
      display.setBackground( Color.BLACK )

      setLayout( new BorderLayout() )
      add( display, BorderLayout.CENTER )

      vis.run( ACTION_COLOR )

      world.addListener( topoListener )
   }

   private def defer( code: => Unit ) {
      EventQueue.invokeLater( new Runnable { def run = code })
   }
   
   def dispose {
      world.removeListener( topoListener )
      stopAnimation
   }

   private def stopAnimation {
      vis.cancel( ACTION_COLOR )
      vis.cancel( ACTION_LAYOUT )
   }

   private def startAnimation {
//      val pParent = pNode.get( PARENT ).asInstanceOf[ PNode ]
//      if( pParent != null ) {
//         val vi   = vis.getVisualItem( GROUP_TREE, pNode )
//         val vip  = vis.getVisualItem( GROUP_TREE, pParent )
//         if( vi != null && vip != null ) {
//            vi.setX( vip.getX )
//            vi.setY( vip.getY )
//         }
//      }
//      vis.run( ACTION_LAYOUT )
      vis.run( ACTION_COLOR )
   }

//   private def insertChild( pNode: PNode, pParent: PNode, info: OSCNodeInfo ) {
//      val pPred = if( info.predID == -1 ) {
//         pParent.set( HEAD, pNode )
//         null
//      } else {
//         map.get( info.predID ) orNull
//      }
//      if( pPred != null ) {
//         pPred.set( SUCC, pNode )
//         pNode.set( PRED, pPred )
//      }
//      val pSucc = if( info.succID == -1 ) {
//         pParent.set( TAIL, pNode )
//         null
//      } else {
//         map.get( info.succID ) orNull
//      }
//      if( pSucc != null ) {
//         pNode.set( SUCC, pSucc )
//         pSucc.set( PRED, pNode )
//      }
//      pNode.set( PARENT, pParent )
//   }

   private def topAddProcs( procs: Proc* ) {
      if( verbose ) println( "topAddProcs : " + procs )
      vis.synchronized {
         stopAnimation
         procs.foreach( topAddProc( _ ))
         startAnimation
      }
   }

   private def topAddProc( p: Proc ) {
      val pNode   = g.addNode()
      val vi = vis.getVisualItem( GROUP_GRAPH, pNode )
//      procG.addTuple( vi )
//      vi.set( VisualItem.SHAPE, Constants.SHAPE_ELLIPSE )
      val aggr = aggrTable.addItem().asInstanceOf[ AggregateItem ]
      aggr.addItem( vi )
      val vProc = ProcTxn.atomic { implicit t =>
         val vParams = p.params.collect {
            case pFloat: ProcParamFloat => {
               val pParamNode = g.addNode()
               val pParamEdge = g.addEdge( pNode, pParamNode )
               val vi = vis.getVisualItem( GROUP_GRAPH, pParamNode )
               aggr.addItem( vi )
               val vFloat = VisualFloat( pFloat, pParamNode, pParamEdge )
               val mVal = p.getFloat( pFloat.name )
               vFloat.value = pFloat.spec.unmap( pFloat.spec.clip( mVal ))
   //            vFloat.mapped = ...
               vi.set( COL_NUAGES, vFloat )
               vFloat
            }
            case pBus: ProcParamAudioBus => {
               val pParamNode = g.addNode()
               val pParamEdge = g.addEdge( pNode, pParamNode )
               val vi = vis.getVisualItem( GROUP_GRAPH, pParamNode )
               vi.set( VisualItem.SIZE, 0.33333f )
               aggr.addItem( vi )
               val vBus = VisualBus( pBus, pParamNode, pParamEdge )
               vi.set( COL_NUAGES, vBus )
               vBus
            }
         }
         val res = VisualProc( p, pNode, aggr, vParams )
         res.playing = p.isPlaying
         res
      }
      vi.set( COL_NUAGES, vProc )
      procMap  += p -> vProc

      // observer
      p.addListener( procListener )
   }

   private def topAddEdges( edges: ProcEdge* ) {
      if( verbose ) println( "topAddEdges : " + edges )
      vis.synchronized {
         stopAnimation
         edges.foreach( e => {
            procMap.get( e.sourceVertex ).foreach( vProcSrc => {
               procMap.get( e.targetVertex ).foreach( vProcTgt => {
                  val outName = e.out.name
                  val inName  = e.in.name
                  val pSrc= vProcSrc.params.find( _.param.name == outName ).map( _.pNode ).getOrElse( vProcSrc.pNode )
                  val pTgt= vProcTgt.params.find( _.param.name == inName  ).map( _.pNode ).getOrElse( vProcTgt.pNode )
                  val pEdge = g.addEdge( pSrc, pTgt )
                  edgeMap += e -> pEdge
               })
            })
         })
         startAnimation
      }
   }
   
   private def topRemoveProcs( procs: Proc* ) {
      if( verbose ) println( "topRemoveProcs : " + procs )
      vis.synchronized {
         stopAnimation
         procs.foreach( p => {
            procMap.get( p ).foreach( topRemoveProc( _ ))
         })
         startAnimation
      }
   }

   private def topRemoveProc( vProc: VisualProc ) {
      val vi = vis.getVisualItem( GROUP_GRAPH, vProc.pNode )
      g.removeNode( vProc.pNode )
//      procG.removeTuple( vi )
//      aggrTable.removeItem( vProc.aggr )
      aggrTable.removeTuple( vProc.aggr ) // XXX OK???
      vProc.params.foreach( vParam => {
         g.removeEdge( vParam.pEdge )
         g.removeNode( vParam.pNode )
      })
      procMap -= vProc.proc
   }

   private def topRemoveEdges( edges: ProcEdge* ) {
      if( verbose ) println( "topRemoveEdges : " + edges )
      vis.synchronized {
         stopAnimation
         edges.foreach( e => {
            edgeMap.get( e ).foreach( pEdge => {
               g.removeEdge( pEdge )
               edgeMap -= e
            })
         })
         startAnimation
      }
   }

   private def topProcPlaying( p: Proc, state: Boolean ) {
      procMap.get( p ).foreach( vProc => {
         vProc.playing = state
         // damageReport XXX
      })
   }
}
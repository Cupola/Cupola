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
import collection.immutable.IntMap
import prefuse.{Constants, Display, Visualization}
import prefuse.action.{RepaintAction, ActionList}
import prefuse.action.animate.{VisibilityAnimator, LocationAnimator, ColorAnimator}
import prefuse.action.assignment.ColorAction
import prefuse.action.layout.graph.{ForceDirectedLayout, NodeLinkTreeLayout}
import prefuse.activity.Activity
import prefuse.render.{DefaultRendererFactory, EdgeRenderer, AbstractShapeRenderer, LabelRenderer}
import prefuse.util.ColorLib
import prefuse.visual.VisualItem
import prefuse.visual.sort.TreeDepthItemSorter
import prefuse.visual.expression.InGroupPredicate
import de.sciss.synth.{Model, Server}
import de.sciss.synth.swing.PrefuseHelper
import de.sciss.synth.proc.{ProcEdge, Proc, ProcDemiurg, ProcWorld}
import prefuse.data.{Edge, Node => PNode, Graph}
import prefuse.controls._

class NuagesPanel( server: Server ) extends JPanel {

   import ProcWorld._

   val vis     = new Visualization()
   val world   = ProcDemiurg.worlds( server )

   private val COL_LABEL            = "name"
   val g                    = {
      val g       = new Graph
      val nodes   = g.getNodeTable()
      g.addColumn( COL_LABEL, classOf[ String ])
//val test = g.addNode()
//test.set( COL_LABEL, "HALLLLLO" )
      g
   }

   var procMap = Map.empty[ Proc, PNode ]
   var edgeMap = Map.empty[ ProcEdge, Edge ]

   // create the tree layout action
   private val orientation = Constants.ORIENT_LEFT_RIGHT
   private val GROUP_GRAPH          = "graph"
   private val GROUP_NODES          = "graph.nodes"
   private val GROUP_EDGES          = "graph.edges"
//   private val GROUP_PAUSED         = "paused"
   private val ACTION_LAYOUT        = "layout"
   private val ACTION_LAYOUT_ANIM   = "layout-anim"
   private val ACTION_COLOR         = "color"
   private val ACTION_COLOR_ANIM    = "layout-anim"
   private val FADE_TIME            = 333

   private val topoListener: Model.Listener = {
      case VerticesRemoved( procs @ _* )  => defer( topRemoveProcs( procs: _* ))
      case VerticesAdded( procs @ _* )    => defer( topAddProcs( procs: _* ))
      case EdgesRemoved( edges @ _* )     => defer( topRemoveEdges( edges: _* ))
      case EdgesAdded( edges @ _* )       => defer( topAddEdges( edges: _* ))
   }
   
   // ---- constructor ----
   {
      val display = new Display( vis )

      vis.add( GROUP_GRAPH, g )
//      vis.addFocusGroup( GROUP_PAUSED, setPaused )

//      val nodeRenderer = new LabelRenderer( COL_LABEL )
      val nodeRenderer = new NuagesProcRenderer
//      nodeRenderer.setRenderType( AbstractShapeRenderer.RENDER_TYPE_FILL )
//      nodeRenderer.setHorizontalAlignment( Constants.LEFT )
//      nodeRenderer.setRoundedCorner( 8, 8 )
//      nodeRenderer.setVerticalPadding( 2 )
//      val edgeRenderer = new EdgeRenderer( Constants.EDGE_TYPE_CURVE )
      val edgeRenderer = new EdgeRenderer( Constants.EDGE_TYPE_LINE, Constants.EDGE_ARROW_FORWARD )

      val rf = new DefaultRendererFactory( nodeRenderer )
      rf.add( new InGroupPredicate( GROUP_EDGES), edgeRenderer )
      vis.setRendererFactory( rf )

      // colors
      val actionNodeColor = new ColorAction( GROUP_NODES, VisualItem.FILLCOLOR, ColorLib.rgb( 200, 200, 200 ))
//      actionNodeColor.add( new InGroupPredicate( GROUP_PAUSED ), ColorLib.rgb( 200, 0, 0 ))
      val actionTextColor = new ColorAction( GROUP_NODES, VisualItem.TEXTCOLOR, ColorLib.rgb( 0, 0, 0 ))

      val actionEdgeColor = new ColorAction( GROUP_EDGES, VisualItem.STROKECOLOR, ColorLib.rgb( 200, 200, 200 ))

      val lay = new ForceDirectedLayout( GROUP_GRAPH )

      // quick repaint
      val actionColor = new ActionList()
      actionColor.add( actionTextColor )
      actionColor.add( actionNodeColor )
      actionColor.add( actionEdgeColor )
//      actionColor.add( actionArrowColor )
      vis.putAction( ACTION_COLOR, actionColor )
      
      val actionLayout = new ActionList( Activity.INFINITY, 50 )
      actionLayout.add( lay )
      actionLayout.add( new RepaintAction() )
      vis.putAction( ACTION_LAYOUT, actionLayout )
//      vis.runAfter( ACTION_COLOR, ACTION_LAYOUT )
      vis.alwaysRunAfter( ACTION_COLOR, ACTION_LAYOUT )

      // ------------------------------------------------

      // initialize the display
      display.setSize( 400, 400 )
      display.setItemSorter( new TreeDepthItemSorter() )
      display.addControlListener( new DragControl() )
      display.addControlListener( new ZoomToFitControl() )
      display.addControlListener( new ZoomControl() )
      display.addControlListener( new WheelZoomControl() )
      display.addControlListener( new PanControl() )
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
println( "topAddProcs : " + procs )
      vis.synchronized {
         stopAnimation
         procs.foreach( p => {
            val pNode = g.addNode()
            procMap += p -> pNode
            pNode.set( COL_LABEL, p.name )
//            pNode.set( COL_LABEL, "GAGAISM" )
//            val vi = vis.getVisualItem( GROUP_GRAPH, pNode )
//            if( vi != null ) {
//               vi.setX( 200 )
//               vi.setY( 200 )
//            }
         })
         startAnimation
      }
   }

   private def topAddEdges( edges: ProcEdge* ) {
println( "topAddEdges : " + edges )
      vis.synchronized {
         stopAnimation
         edges.foreach( e => {
            procMap.get( e.sourceVertex ).foreach( pNodeSrc => {
               procMap.get( e.targetVertex ).foreach( pNodeTgt => {
                  val pEdge = g.addEdge( pNodeSrc, pNodeTgt )
                  edgeMap += e -> pEdge
               })
            })
         })
         startAnimation
      }
   }
   
   private def topRemoveProcs( procs: Proc* ) {
println( "topRemoveProcs : " + procs )
      vis.synchronized {
         stopAnimation
         procs.foreach( p => {
            procMap.get( p ).foreach( pNode => {
               g.removeNode( pNode )
               procMap -= p
            })
         })
         startAnimation
      }
   }

   private def topRemoveEdges( edges: ProcEdge* ) {
println( "topRemoveEdges : " + edges )
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
}
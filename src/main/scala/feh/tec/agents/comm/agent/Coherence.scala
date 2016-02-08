package feh.tec.agents.comm.agent

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.Future
import scala.language.higherKinds

/** Abstract <b>coherence</b> definition.
 *  It is based on coherence assessment of <i>information graphs</i> throughout the contexts.
 */
trait Coherence{

  /** A node of information graphs. */
  type InformationPiece
  /** A set of nodes, with graph-like relations calculation support. */
  type Graph <: Equals


  /** Base <b>context</b> trait.
    *
    * @tparam C self type reference.
    */
  sealed trait AbstractContext[C <: AbstractContext[C]] {
    self: C =>

    /** Input type for [[process]]. */
    type Input
    /** Result type of [[process]]. */
    type Result

    /** Process the input. */
    def process: Input => Result

  }

  /** A normal context, as described in [*].
    *
    * @tparam C self type reference.
    */
  trait Context[C <: Context[C]] extends AbstractContext[C]{
    self: C =>

    /** Coherence value. */
    type Value

    type Input = Graph
    /** The [[process]] result is asynchronous to avoid deadlocks. */
    type Result = Future[Seq[SolutionCandidate[C]]]

    /** Base binary relation. */
    trait RelationBinary extends ((InformationPiece, InformationPiece) => Value)
    /** Base relation on whole [[Graph]]. */
    trait RelationWhole  extends (Graph => Value)

    /** The context's default inf. graph. See [*]. */
    def defaultGraph: Graph

    def binaryRelations: Set[RelationBinary]
    def wholeRelations:  Set[RelationWhole]

  }


  /** A (mutable) context, accumulating the input values rather than giving an immediate result.
    *
    * @tparam C self type reference.
    */
  trait AccumulatingContext[C <: AccumulatingContext[C]] extends AbstractContext[C]{
    self: C =>

    type AResult

    type Result = Unit

    def accumulate: Seq[Input] => Unit

    def getAccumulated: Seq[Input]
    def resetAccumulated()

    def processAccumulated(): AResult
  }

  /** An implementation of the accumulation.
    *
    * @tparam C self type reference.
    */
  trait AccumulatingContextImpl[C <: AccumulatingContext[C]] extends AccumulatingContext[C]{
    self: C =>

    private var _acc = mutable.Buffer.empty[Input]

    def process = g => {
      _acc += g
    }
    def accumulate = _acc ++= _
    def getAccumulated = _acc.toSeq
    def resetAccumulated() = _acc.clear()
  }

  // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  /** A container for <i>information graphs</i> with history over the [[Context]]s.
    * @tparam C last entry's [[Context]].
    */
  sealed trait SolutionCandidate[C <: Context[C]]

  /** A <b>success</b>ful [[SolutionCandidate]].
    *
    * @param g inf. graph on the exit from the last context.
    * @param c last context
    * @param v coherence value, received at the context.
    * @param previous previous <i>success</i> entries.
    * @tparam C last entry's [[Context]].
    */
  case class SolutionSuccess[C <: Context[C]](g: Graph,
                                              c: C,
                                              v: C#Value,
                                              previous: List[SolutionSuccess[_]]) extends SolutionCandidate[C]

  /** A <b>fail</b>ed [[SolutionCandidate]].
    *
    * @param g failed inf. graph.
    * @param failedAt context fallen at.
    * @param reason failure reason.
    * @param previous previous <i>success</i> entries.
    * @tparam C last entry's [[Context]].
    */
  case class SolutionFailure[C <: Context[C]](g: Graph,
                                              failedAt: C,
                                              reason: Any,
                                              previous: List[SolutionSuccess[_]] ) extends SolutionCandidate[C]


 // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  /** Divide an information graph into acceptable sub-graphs in the given context.
    *
    * @param g information graph to process.
    * @param c the context.
    * @param threshold acceptability threshold.
    * @tparam C context.
    * @return [[SolutionCandidate]] result containers.
    */
  protected def divideInfGraph[C <: Context[C]](g: Graph, c: C, threshold: Double): Seq[SolutionCandidate[C]]

 // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  /** A [[Graph]] should provide the following methods: */
  trait GraphOps{

    /** Extract a sub-graph.
      *
      * @param select select the nodes.
      * @return a new [[Graph]] with the selected nodes.
      */
    def subGraph(select: InformationPiece => Boolean): Graph

    /** Join two graphs together.
      *
      * @param that the 2nd [[Graph]] to join.
      * @return a new [[Graph]], containing the nodes of both.
      */
    def :+: (that: Graph): Graph

    /** Add new nodes.
      *
      * @param inf [[InformationPiece]]s to add.
      * @return a new [[Graph]] with the new information aggregated.
      */
    def +# (inf: InformationPiece*): Graph

    /** Retrieve the nodes.
      *
      * @return the nodes set.
      */
    def nodes: Set[InformationPiece]

    /** Retrieve the nodes, lazy.
      *
      * @return a lazy sequence of nodes.
      */
    def nodesLazy: Stream[InformationPiece]

    /** Retrieve all possible node pairs, lazy.
      *
      * @return a lazy sequence of node pairs.
      */
    def allNodesPairs: Stream[(InformationPiece, InformationPiece)] =
      for {
        x <- nodesLazy
        y <- nodesLazy
        if x != y
      } yield (x, y)


    /** Apply a function to each node returning the result.
      *
      * @param f function to apply.
      * @param builder implicit build evidence.
      * @tparam C result container.
      * @tparam R result value.
      */
    def map[C[_], R](f: InformationPiece => R)
                    (implicit builder: CanBuildFrom[Seq[R], R, C[R]]): C[R] =
    {
      val b = builder()
      b ++= nodes.toSeq.map(f)
      b.result()
    }

    /** Applies a binary operator to a start value and all the nodes (going left to right).
      *
      * @param z start value.
      * @param op binary operator to apply.
      * @tparam R result.
      */
    def foldLeft[R](z: R)(op: (R, InformationPiece) => R): R =  {
      var acc = z
      for (n <- nodes) acc = op(acc, n)
      acc
    }

    /** Calculate binary relations on nodes, lazy.
      *
      * @param rels binary relations functions.
      * @tparam R result.
      */
    def relationsOn[R](rels: Seq[(InformationPiece, InformationPiece) => R]): Stream[((InformationPiece, InformationPiece), Seq[R])] =
      allNodesPairs.map(i => (i, rels.map(_.tupled(i))))
  }


  /** An implicit conversion of a [[Graph]] to a [[GraphOps]]. */
  implicit def graphOps: Graph => GraphOps


  /** Creates an new [[Graph]] instance.
    *
    * @param nodes initial nodes.
    */
  def newGraph(nodes: TraversableOnce[InformationPiece]): Graph
  def newGraph(nodes: InformationPiece*): Graph = newGraph(nodes)

  def emptyGraph: Graph
}


object Coherence{

/*
  trait GraphImplementation extends Coherence{

    type Graph = GraphNodes

    case class GraphNodes(nodes: Set[InformationPiece])

    class GraphNodesOps(g: GraphNodes) extends GraphOps{
      type SelectSubGraph = InformationPiece => Boolean

      def subgraph(select: SelectSubGraph) = GraphNodes(g.nodes.filter(select))

      def :+:(that: GraphNodes) = GraphNodes(g.nodes ++ that.nodes)
    }

    implicit def graphOps = new GraphNodesOps(_)
  }
*/

}
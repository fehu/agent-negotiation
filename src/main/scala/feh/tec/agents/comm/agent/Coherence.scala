package feh.tec.agents.comm.agent

import feh.util._

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, higherKinds}

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

    /** Coherence context value. */
    type Value

    def toDouble: C#Value => Option[Double]

    type Input = Graph
    /** The [[process]] result is asynchronous to avoid deadlocks. */
    type Result = Future[Seq[ThisSolutionCandidate[C]]]

    implicit def ex: ExecutionContext

    /** The context's default inf. graph. See [*]. */
    def defaultGraph: Graph

    /** Relations between the nodes of the assessed graph. */
    def binaryRelationsWithin: Set[RelationBinary[C]]
    /** Relations between the nodes of the assessed graph and the nodes of the default graph. */
    def binaryRelationsWithDefault: Set[RelationBinary[C]]
    /** Relations assessing the whole graph. */
    def wholeRelations:  Set[RelationWhole[C]]

  }

  /** Base binary relation. */
  trait RelationBinary[C <: Context[C]] extends ((InformationPiece, InformationPiece) => C#Value)
  /** Base relation on whole [[Graph]]. */
  trait RelationWhole[C <: Context[C]]  extends (Graph => C#Value)



  case class ContextContainer(c: C forSome{ type C <: Context[C]} )
  implicit def contextToContainer[C <: Context[C]](c: C): ContextContainer = ContextContainer(c)

  /** Propagates the initial solutions through the given contexts. Asynchronous. */
  def propagateSolutions(initial: Set[SomeSolutionSuccess], contexts: Seq[ContextContainer])
                        (implicit ex: ExecutionContext): Future[Set[SolutionCandidate[_]]] =
    Y[(List[ContextContainer], Set[SomeSolutionSuccess], Set[SolutionFailure[_]]), Future[Set[SolutionCandidate[_]]]](
      rec => {
        case (ContextContainer(c) :: cs, successful, failed) =>
          graphThroughContext(successful, c) flatMap {
            case (succ, fail) =>
              rec((cs, succ.map(SomeSolutionSuccess(_)), failed ++ fail))
          }
        case (Nil, successful, failed) => Future { ((successful.map(_.get).toSeq : Seq[SolutionCandidate[_]]) ++ failed.toSeq).toSet }
      }
    )((contexts.toList, initial, Set()))


  protected def graphThroughContext[C <: Context[C]](in: Set[SomeSolutionSuccess], c: C)
                                                    (implicit ex: ExecutionContext): Future[(Set[SolutionSuccess[C]], Set[SolutionFailure[C]])] =
    Future.sequence(in.toSeq.map{
      lastSuccess =>
        c.process(lastSuccess.g).map(_.map(_.withPrevious(lastSuccess)))
    })
    .map{
      results =>
        val (succ, fail) = results.flatten.partition(_.isSuccess)
        succ.toSet.flatMap((_: SolutionCandidate[C]).success) -> fail.toSet.flatMap((_: SolutionCandidate[C]).failure)
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
  sealed trait SolutionCandidate[C <: Context[C]]{
    def isSuccess: Boolean
    final def isFailure = !isSuccess

    final def success = if(isSuccess) Some(this.asInstanceOf[SolutionSuccess[C]]) else None
    final def failure = if(isFailure) Some(this.asInstanceOf[SolutionFailure[C]]) else None
  }

  /** A <b>success</b>ful [[SolutionCandidate]].
    *
    * @param g inf. graph on the exit from the last context.
    * @param c last context
    * @param v coherence (0, 1].
    * @param previous previous <i>success</i> entries.
    * @tparam C last entry's [[Context]].
    */
  case class SolutionSuccess[C <: Context[C]](g: Graph,
                                              c: C,
                                              v: InUnitInterval.Excluding0,
                                              previous: Option[SomeSolutionSuccess]) extends SolutionCandidate[C]
  {
    final def isSuccess = true
  }

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
                                              previous: Option[SomeSolutionSuccess] ) extends SolutionCandidate[C]
  {
    final def isSuccess = false
  }


  case class SomeSolutionCandidate(get: SolutionCandidate[C] forSome{ type C <: Context[C]})
  case class SomeSolutionSuccess(get: SolutionSuccess[C] forSome{ type C <: Context[C]})

  implicit def SolutionCandidate2SomeSolutionCandidate[S <: SolutionCandidate[_]](s: S): SomeSolutionCandidate =
    SomeSolutionCandidate(s.asInstanceOf)
  implicit def GetSomeSolutionSuccess(s: SomeSolutionSuccess): SolutionSuccess[C] forSome{ type C <: Context[C]} = s.get


  sealed trait ThisSolutionCandidate[C <: Context[C]]{
    def isSuccess: Boolean
    final def isFailure = !isSuccess

    final def success = if(isSuccess) Some(this.asInstanceOf[ThisSolutionCandidate.ThisSolutionSuccess[C]]) else None
    final def failure = if(isFailure) Some(this.asInstanceOf[ThisSolutionCandidate.ThisSolutionFailure[C]]) else None

    def withPrevious(prev: SomeSolutionSuccess): SolutionCandidate[C]
    def withNoPrevious: SolutionCandidate[C]
  }

  object ThisSolutionCandidate{
    case class ThisSolutionSuccess[C <: Context[C]](g: Graph,
                                                    c: C,
                                                    v: InUnitInterval.Excluding0) extends ThisSolutionCandidate[C]{
      final def withPrevious(prev: SomeSolutionSuccess): SolutionSuccess[C] = SolutionSuccess(g, c, v, Option(prev))
      final def withNoPrevious = withPrevious(null)
      final def isSuccess = true
    }

    case class ThisSolutionFailure[C <: Context[C]](g: Graph,
                                                    failedAt: C,
                                                    reason: Any) extends ThisSolutionCandidate[C]{
      final def withPrevious(prev: SomeSolutionSuccess): SolutionFailure[C] = SolutionFailure(g, failedAt, reason, Option(prev))
      final def withNoPrevious = withPrevious(null)
      final def isSuccess = false
    }
  }


 // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // // //

  /** Graph partition interface. */ // todo: [C <: Context[C]] must be in method's definition
  trait GraphPartition[C <: Context[C]]{
    def acceptablePartitions(g: Graph, c: C, threshold: Double): Set[ThisSolutionCandidate.ThisSolutionSuccess[C]]
  }

  /** Coherence assessment interface. */
  trait CoherenceAssessment{
    /** Assesses inner coherence of a subgraph A - without considering the coherence with U / A .
      * If any coherence value is found to be negative, 0 (zero) is returned.
      * Any relation with 0 coherence value is ignored.
      *
      * @param c context
      * @param subGraph subgraph to assess (A).
      * @return a [[Double]] value in [0, 1] interval.
      */
    def assessCoherence[C <: Context[C]](c: C, subGraph: Graph): InSomeUnitInterval

    /** Assesses coherence of a subgraph A, that is part of graph U.
      * Considers both inner (withing A) and outer (between A and U / A) coherence.
      * If any inner coherence value is found to be negative, 0 (zero) is returned.
      * For outer coherence rules see [*].
      * Any relation with 0 coherence value is ignored.
      *
      * @param c context
      * @param graph owner graph U.
      * @param subGraph subgraph A to assess.
      * @return a [[Double]] value in [0, 1] interval.
      */
    def assessCoherence[C <: Context[C]](c: C, graph: Graph, subGraph: Graph): InSomeUnitInterval
  }

  /** Divide an information graph into acceptable sub-graphs in the given context.
    *
    * @param g information graph to process.
    * @param c the context.
    * @param threshold acceptability threshold.
    * @tparam C context.
    * @return [[SolutionCandidate]] result containers.
    */
  def divideInfGraph[C <: Context[C]](g: Graph, c: C, threshold: Double)
                                     (implicit pMethod: GraphPartition[C]): Set[ThisSolutionCandidate.ThisSolutionSuccess[C]] =
    pMethod.acceptablePartitions(g, c, threshold)


  object GraphPartition{

    /** Encounters all (non-repeating) acceptable information graph partitions by aggregation. See [*].
      *
      * @tparam C
      */
    class GraphPartitionByAggregation[C <: Context[C]](implicit cAssess: CoherenceAssessment) extends GraphPartition[C]
    {
      def acceptablePartitions(g: Graph, c: C, threshold: Double) = {
        val rels  = c.binaryRelationsWithin.toSeq
        val size1 = g.nodes.map(newGraph(_))

        val graphs = Y[(Set[Graph], Set[Graph]), Set[Graph]](
          rec => {
            case (acc, last) =>
              // try to join `last` with `size1`
              val nextJoin = for{
                g0@GraphNodes(nodesLast)   <- last
                g1@GraphNodes(Seq(n1)) <- size1
                bRels <- nodesLast.map(g => rels.map(_(g, n1)))
                if bRels.forall(c.toDouble andThen (_.exists(_ > threshold)))
              } yield g0 :+: g1

              if (nextJoin.nonEmpty) rec(acc ++ last, nextJoin)
              else acc ++ last
          }
        )(Set.empty[Graph] -> size1)

        graphs.map{
          g => ThisSolutionCandidate.ThisSolutionSuccess(g, c, cAssess.assessCoherence(c, g).excluding0)
        }
      }
    }
  }

  object CoherenceAssessment{
    class CoherenceAssessmentImpl/*(reduceBinary: Seq[C#Value forSome {type C <: Context[C]}] => InUnitInterval )*/
      extends CoherenceAssessment
    {

      /** assess coherence of:
        * - binary within: foreach pair of nodes - calc all the relations, find mean.
        * - binary with default: the same.
        * - whole: foreach relation - calc coherence and find product.
        * return the product of both binaries and whole.
        *
        * * in all cases: a relation value <= 0 results in 0 returned
        */
      def assessCoherence[C <: Context[C]](c: C, subGraph: Graph): InSomeUnitInterval = {


        val binariesIn0 = subGraph.relationsWithin(c.binaryRelationsWithin.toSeq).map(_._2.map(c.toDouble))
        if (binariesIn0.exists(_.exists(_.exists(_ <= 0)))) return InUnitInterval.Including(0)
        val binariesIn = binariesIn0.map(_.flatten.product)
        val binaryIn = binariesIn.sum / binariesIn.length

        val binariesDG0 = subGraph.relationsWith(c.defaultGraph)(c.binaryRelationsWithDefault.toSeq).map(_._2.map(c.toDouble))
        if (binariesDG0.exists(_.exists(_.exists(_ <= 0)))) return InUnitInterval.Including(0)
        val binariesDG = binariesDG0.map(_.flatten.product)
        val binaryDG = binariesDG.sum / binariesDG.length


        val whole0 = c.wholeRelations.map(c toDouble _(subGraph))
        if(whole0.exists(_.exists(_ <= 0))) return InUnitInterval.Including(0)
        val whole = whole0.flatten.product

        InUnitInterval Excluding0 binaryIn*binaryDG*whole
      }

      /** 1. assesses inner coherence with `assessCoherence`.
        * 2. assesses outer coherence:
        *
        */
      def assessCoherence[C <: Context[C]](c: C, graph: Graph, subGraph: Graph): InSomeUnitInterval = {
        val inner = assessCoherence(c, subGraph)
        if(inner.doubleValue == 0) return InUnitInterval.Including(0)

        val complement = graph - subGraph
        val outer = subGraph.relationsWith[Double](complement)(Seq({
          case (x, y) =>
            val cohs = c.binaryRelationsWithin.toSeq.map(c toDouble _(x, y) match {
              case Some(d) if d < 0 => -d
              case _                => 0d
            }).filterNot(0 == _)
            if(cohs.nonEmpty) cohs.sum / cohs.length else 0d
        }))
        InUnitInterval.Excluding0(outer.map(_._2.head).sum / outer.length)
      }
    }
  }

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

    /** Find the difference of two graphs.
      *
      * @param that another graph
      * @return graph with nodes of `this` that are not in `that`
      */
    def -(that: Graph): Graph

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

    type RelationsResult[R] = ((InformationPiece, InformationPiece), Seq[R])

    /** Calculate binary relations on graph nodes, lazy.
      *
      * @param rels binary relations functions.
      * @tparam R result.
      */
    def relationsWithin[R](rels: Seq[(InformationPiece, InformationPiece) => R]): Stream[RelationsResult[R]] =
      allNodesPairs.map(i => (i, rels.map(_.tupled(i))))

    /** Calculate binary relations between two graphs.
      *
      * @param that another graph.
      * @param rels binary relations functions.
      */
    def relationsWith[R](that: Graph)(rels: Seq[(InformationPiece, InformationPiece) => R]): Stream[RelationsResult[R]] =
      for {
        nThis <- this.nodesLazy
        nThat <- that.nodesLazy
      } yield (nThis -> nThat, rels.map(_(nThis, nThat)))

  }

  /** Nodes extractor. */
  object GraphNodes{
    def unapply(g: Graph): Option[Seq[InformationPiece]] = Some(g.nodes.toStream)
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

  trait GraphImplementation {
    self: Coherence =>

    type Graph = GraphImpl

    case class GraphImpl(nodes: Set[InformationPiece])

    class GraphNodesOps(g: GraphImpl) extends GraphOps{

      def subGraph(select: (InformationPiece) => Boolean) = GraphImpl(g.nodes.filter(select))

      def :+:(that: GraphImpl) = GraphImpl(g.nodes ++ that.nodes)
      def +#(inf: InformationPiece*) = GraphImpl(g.nodes ++ inf)
      def -(that: Graph) = GraphImpl(g.nodes -- that.nodes)

      def nodes = g.nodes
      def nodesLazy = g.nodes.toStream

    }

    implicit def graphOps = new GraphNodesOps(_)

    def newGraph(nodes: TraversableOnce[InformationPiece]) = GraphImpl(nodes.toSet)
    def emptyGraph = GraphImpl(Set())
  }

}
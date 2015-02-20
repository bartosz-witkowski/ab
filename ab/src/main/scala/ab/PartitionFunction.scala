package ab

import ab.model._

trait PartitionFunction {
  def partition(experiment: Experiment): Group
}

object PartitionFunction {
  def apply(f: Experiment => Group): PartitionFunction = new PartitionFunction {
    override def partition(experiment: Experiment): Group = f(experiment)
  }

  def from(groups: Groups, hash: HashFunction): PartitionFunction = PartitionFunction { experiment =>
    val l = hash.hashFor(experiment)
    groups.groupFor(l)
  }
}

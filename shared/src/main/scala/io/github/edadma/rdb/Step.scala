package io.github.edadma.rdb

trait Step {}

case class ScanStep(tab: Table) extends Step

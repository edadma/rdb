package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import pprint.*

trait Testing:
  def test(sql: String): String =
    given Session = new MemoryDB().connect()

    PPrinter.BlackWhite(executeSQL(sql)).toString :+ '\n'

  def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()

    executeSQL(sql).collect { case QueryResult(t) => t }.last

  def results(sql: String): Seq[io.github.edadma.petradb.Result] =
    given Session = new MemoryDB().connect()

    executeSQL(sql)

  def setupSession(sql: String): Session =
    given session: Session = new MemoryDB().connect()
    executeSQL(sql)
    session

  def findProcess[T](proc: Process)(pf: PartialFunction[Process, T]): Option[T] =
    if pf.isDefinedAt(proc) then Some(pf(proc))
    else
      proc match
        case p: ProjectProcess                    => findProcess(p.input)(pf)
        case p: SeqScanProcess                    => findProcess(p.input)(pf)
        case p: SortProcess                       => findProcess(p.input)(pf)
        case p: AggregateProcess                  => findProcess(p.input)(pf)
        case p: TakeProcess                       => findProcess(p.input)(pf)
        case _: DropProcess                       => None
        case p: DistinctProcess                   => findProcess(p.input)(pf)
        case p: HavingProcess                     => findProcess(p.input)(pf)
        case p: AliasProcess                      => findProcess(p.input)(pf)
        case p: ColumnAliasProcess                => findProcess(p.input)(pf)
        case p: CrossProcess                      => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: LeftCrossJoinProcess              => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: RightCrossJoinProcess             => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: FullCrossJoinProcess              => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: IndexNestedLoopJoinProcess        => findProcess(p.outer)(pf)
        case p: LeftIndexNestedLoopJoinProcess    => findProcess(p.outer)(pf)
        case p: RightIndexNestedLoopJoinProcess   => findProcess(p.outer)(pf)
        case p: HashJoinProcess                   => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: LeftHashJoinProcess               => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: RightHashJoinProcess              => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: FullHashJoinProcess               => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: WindowProcess                     => findProcess(p.input)(pf)
        case _                                    => None

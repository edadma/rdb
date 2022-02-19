package io.github.edadma.rdb

trait Command

case class CreateDatabaseCommand(name: String) extends Command

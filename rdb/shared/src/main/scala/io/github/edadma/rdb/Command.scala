package io.github.edadma.rdb

trait TableConstraint:
  def name: Option[String]

case class UniqueConstraint(name: Option[String], columns: Seq[Ident]) extends TableConstraint
case class PrimaryKeyConstraint(name: Option[String], columns: Seq[Ident]) extends TableConstraint
case class ForeignKeyConstraint(name: Option[String], columns: Seq[Ident], referencedTable: Ident, referencedColumns: Seq[Ident]) extends TableConstraint

trait Command

case class QueryCommand(query: Expr) extends Command
case class InsertCommand(table: Ident, columns: Option[Seq[Ident]], rows: Seq[Seq[Expr]], returning: Option[Ident])
    extends Command
case class CreateTableCommand(table: Ident, columns: Seq[ColumnDesc], constraints: Seq[TableConstraint]) extends Command
case class CreateEnumCommand(name: Ident, labels: Seq[String])                                           extends Command
case class UpdateCommand(table: Ident, sets: Seq[UpdateSet], cond: Option[Expr])                         extends Command
case class DeleteCommand(table: Ident, cond: Option[Expr])                                               extends Command
case class AlterTableCommand(table: Ident, alter: TableAlteration)                                       extends Command
case class DropTableCommand(table: Ident, ifExists: Boolean = false, cascade: Boolean = false)         extends Command
case class DropIndexCommand(name: Ident, ifExists: Boolean = false)                                    extends Command  
case class DropTypeCommand(name: Ident, ifExists: Boolean = false, cascade: Boolean = false)          extends Command
case object BeginCommand    extends Command
case object CommitCommand   extends Command
case object RollbackCommand extends Command

case class UpdateSet(col: Ident, value: Expr)
case class ColumnDesc(
    name: Ident,
    typ: Either[Type, Ident],
    required: Boolean,
    unique: Boolean,
    default: Option[Expr],
    references: Option[(Ident, Ident)], // Single column foreign key: (table, column)
)

trait TableAlteration

case class AddColumnTableAlteration(column: ColumnDesc) extends TableAlteration
case class DropColumnTableAlteration(column: Ident) extends TableAlteration
case class AlterColumnTableAlteration(column: Ident, modification: ColumnModification) extends TableAlteration
case class AddConstraintTableAlteration(constraint: TableConstraint) extends TableAlteration
case class DropConstraintTableAlteration(name: Ident) extends TableAlteration
case class RenameTableAlteration(newName: Ident) extends TableAlteration
case class RenameColumnTableAlteration(oldName: Ident, newName: Ident) extends TableAlteration

// Legacy - keep for backward compatibility
case class AddForeignKeyTableAlteration(fk: Ident, ref: Ident) extends TableAlteration
case class AddForeignKeyConstraintTableAlteration(constraint: ForeignKeyConstraint) extends TableAlteration

trait ColumnModification
case class SetDataTypeColumnModification(newType: Either[Type, Ident]) extends ColumnModification
case class SetNotNullColumnModification() extends ColumnModification
case class DropNotNullColumnModification() extends ColumnModification
case class SetDefaultColumnModification(default: Expr) extends ColumnModification
case class DropDefaultColumnModification() extends ColumnModification

import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

lazy val db = Database.forURL("jdbc:sqlite:persons.sqlite", driver = "org.sqlite.JDBC")
case class Person(first: String, id: Long = 0L)

class PersonTable(tag: Tag) extends Table[Person](tag, "persons") {
  def first = column[String]("first")
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def * = (first, id) <> (Person.tupled, Person.unapply)
}

lazy val PersonTable = TableQuery[PersonTable]

val createTablesAction = db.run(PersonTable.schema.create)
Await.result(createTablesAction, Duration.Inf)

val insertB = db.run(PersonTable += Person("Barry"))

Await.result(insertB, Duration.Inf)

val q1 = PersonTable.filter(_.first === "Barry")
val result = Await.result(db.run(q1.result), Duration.Inf)

result map (_.first)

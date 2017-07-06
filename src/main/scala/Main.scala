import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


// If using the auto-increment, best to set the id as an Option[Long]
case class Person(first: String, id: Option[Long] = None)

// I'm not ure what's considered proper, but I declare the variables as Rep[T],
// and let the columns figure it out on thier own :-)
class PersonTable(tag: Tag) extends Table[Person](tag, "persons") {
  def first: Rep[String] = column("first")
  def id: Rep[Option[Long]] = column("id", O.PrimaryKey, O.AutoInc)

  def * = (first, id) <> (Person.tupled, Person.unapply)
}

// I usually put queries in a companion object for the table
// as well as any other pre-define actions I'd do a lot
object PersonTable {

  val personQuery = TableQuery[PersonTable]

  def createScheme = personQuery.schema.create

  // .result is what makes the magic happen ;-)
  def findByFirst(fn: String) = personQuery.filter(_.first).result
  def getAll = personQuery.result
  // However, you can chain queries if you call .result wherver you are running it, instead of the definition

  def startsWithPredicate(aLetter: String): PersonTable => Rep[Boolean] = somePerson => somePerson.first.toLowerCase.startsWith(aLetter.toLowerCase)
  def endsWithPredicate(aLetter: String): PersonTable => Rep[Boolean] = somePerson => somePerson.first.toLowerCase.endsWith(aLetter.toLowerCase)
  def nameLengthPredicate(nameLength: Int): PersonTable => Rep[Boolean] = somePerson => somePerson.first.length <= nameLength

  def peopleProbablyNamedMark = personQuery.filter(nameLengthPredicate(4)).filter(startsWithPredicate("M")).filter(endsWithPredicate("k"))

  // can also write it like this
  def peopleIKnow(sw: String, ew: String, len: Int): PersonTable => Rep[Boolean] = somePerson => {

    // Complex/dynamic SQL logic here in real life :-)
    List (
      somePerson.first.toLowerCase.startsWith(sw.toLowerCase),
      somePerson.first.toLowerCase.endsWith(ew.toLowerCase),
      somePerson.first.length <= len
    ).reduceLeftOption(_ || _).getOrElse(false: Rep[Boolean])

  }


}

object Main extends App {


//  val db = Database.forURL("jdbc:sqlite:persons.sqlite", driver = "org.sqlite.JDBC")
//  val createTablesAction = db.run(PersonTable.schema.create)
//
//  Await.result(createTablesAction, Duration.Inf)
//
//  val insertB = db.run(PersonTable += Person("Barry"))
//
//  Await.result(insertB, Duration.Inf)
//
//  val q1 = PersonTable.filter(_.first === "Barry")
//  val result = Await.result(db.run(q1.result), Duration.Inf)
//
//  result map (_.first)


}

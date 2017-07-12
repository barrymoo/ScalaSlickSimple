import java.sql.SQLException

import org.sqlite.SQLiteException
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration


// If using the auto-increment, best to set the id as an Option[Long]
case class Person(first: String, id: Option[Long] = None)

// I'm not ure what's considered proper, but I declare the variables as Rep[T],
// and let the columns figure it out on their own :-)
class PersonTable(tag: Tag) extends Table[Person](tag, "persons") {
  def first: Rep[String] = column("first")
  def id: Rep[Option[Long]] = column("id", O.PrimaryKey, O.AutoInc)

  def * = (first, id) <> (Person.tupled, Person.unapply)
}

// I usually put queries in a companion object for the table
// as well as any other pre-define actions I'd do a lot
object PersonTable {

  val personQuery = TableQuery[PersonTable]

  def createSchema = personQuery.schema.create

  // .result is what makes the magic happen ;-)
  def findByFirst(fn: String) = personQuery.filter(_.first === fn).result
  def getAll = personQuery.result
  // However, you can chain queries if you call .result where you are running it, instead of the definition
  // .result is like 'I've composed my query, now transpile it to sql'

  def startsWithPredicate(aLetter: String): PersonTable => Rep[Boolean] = somePerson => somePerson.first.toLowerCase.startsWith(aLetter.toLowerCase)
  def endsWithPredicate(aLetter: String): PersonTable => Rep[Boolean] = somePerson => somePerson.first.toLowerCase.endsWith(aLetter.toLowerCase)
  def nameLengthPredicate(nameLength: Int): PersonTable => Rep[Boolean] = somePerson => somePerson.first.length <= nameLength

  def peopleProbablyNamedMark = personQuery.filter(nameLengthPredicate(4)).filter(startsWithPredicate("M")).filter(endsWithPredicate("k"))

  // You can also write it like this
  def peopleIKnow(sw: String, ew: String, len: Int): PersonTable => Rep[Boolean] = somePerson => {

    // Complex/dynamic SQL logic here in real life :-)
    List (
      somePerson.first.toLowerCase.startsWith(sw.toLowerCase),
      somePerson.first.toLowerCase.endsWith(ew.toLowerCase),
      somePerson.first.length <= len
    ).reduceLeftOption(_ && _).getOrElse(false: Rep[Boolean])
    // && so only true if everything in the list is true

  }

  def peopleIMightKnow(sw: String, ew: String, len: Int): PersonTable => Rep[Boolean] = somePerson => {

    // Complex/dynamic SQL logic here in real life :-)
    List (
      somePerson.first.toLowerCase.startsWith(sw.toLowerCase),
      somePerson.first.toLowerCase.endsWith(ew.toLowerCase),
      somePerson.first.length <= len
    ).reduceLeftOption(_ || _).getOrElse(false: Rep[Boolean])
    // Note the OR instead of the AND above, so it's true if anything in the list is true
  }

  // You can get crazy with having Sub lists you map so effectively make AND(_, OR(_,_)) type queries

}

object Main extends App {


  val db = Database.forURL("jdbc:sqlite:persons.sqlite", driver = "org.sqlite.JDBC")

  println("Creating table...")
  val createTablesAction = db.run(PersonTable.createSchema).map(_ => println("Table created!")).recover {
    case sql: SQLiteException => {
      // Should check code, but we'll just assume the table already exists error
      // and therefore can just move on
      println("Table exists!")
    }
    case ex : Exception => {
      println("Something went terribly wrong!")
      println(ex.getMessage)
      throw new RuntimeException("Borked!")
    }
  }
  Await.result(createTablesAction, Duration.Inf)

  println("\n\n\n")

  // Insert names if none are present
  val insertFuture = db.run(PersonTable.personQuery.length.result).flatMap {

    case 0 => {
      val records = Seq("Barry", "Mark", "Tina", "Bob", "Jackie", "Mork", "Thomas", "Eleanor", "Barry", "Tina", "Bobby")
      println("Inserting sample records...")
      // Bulk insert
      db.run(DBIO.seq(PersonTable.personQuery ++= records.map(n => Person(n)))).map{ _ =>
        println("Sample records inserted!\n\n\n")
      }
    }

    case _ => {
      println("Names already in the database table!\n\n\n")
      Future(Unit)
    }
  }


  Await.result(insertFuture, Duration.Inf)

  println("Running sample queries...")

  // We'll just chain futures here with flatMap, so we only have one await
  val allTheFutures = db.run(PersonTable.getAll).flatMap { resultSet =>

    println("Here are the people in the database...")
    resultSet.foreach(p => println(p.toString))
    println("\n\n\n")

    // Run the next query
    db.run(PersonTable.findByFirst("Barry"))
  }.flatMap { resultSet =>

    // Case sensitive ;-)
    println("Here are the people named 'Barry'")
    resultSet.foreach(p => println(p.toString))
    println("\n\n\n")

    // Run the next query
    db.run(PersonTable.peopleProbablyNamedMark.result) // Call .result here
  }.flatMap { resultSet =>
    // Case sensitive ;-)
    println("Here are the people probably named mark")
    resultSet.foreach(p => println(p.toString))
    println("\n\n\n")

    // Run the next query
    db.run(PersonTable.personQuery.filter(PersonTable.peopleIKnow("b", "y", 5)).result) // This was a predicate, so we call it in a filter
  }.flatMap { resultSet =>
    // Case sensitive ;-)
    println("Here are the people whose name starts with a 'b', ends with a 'y', AND has 5 or less letters")
    resultSet.foreach(p => println(p.toString))
    println("\n\n\n")

    // Run the next query
    db.run(PersonTable.personQuery.filter(PersonTable.peopleIMightKnow("b", "a", 0)).result)
  }.flatMap { resultSet =>

    // Case sensitive ;-)
    println("Here are the people whose name starts with a 'b' OR ends with an 'a'")
    resultSet.foreach(p => println(p.toString))
    println("\n\n\n")

    Future.successful(Unit)// All done, but we need to flatMap into a Future. Could just map instead of flatMap...
  }


  Await.result(allTheFutures, Duration.Inf)

  println("All done! Shutting down database connection...")
  db.close()

}

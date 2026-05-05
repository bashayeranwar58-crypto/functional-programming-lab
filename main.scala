// Reader - для конфигурации библиотеки
case class Reader[E, A](run: E => A) {
  def map[B](f: A => B): Reader[E, B] =
    Reader(env => f(run(env)))

  def flatMap[B](f: A => Reader[E, B]): Reader[E, B] =
    Reader(env => f(run(env)).run(env))
}

object Reader {
  def pure[E, A](a: A): Reader[E, A] = Reader(_ => a)
}

// Writer - для логирования операций
case class Writer[A](value: A, log: Vector[String]) {
  def map[B](f: A => B): Writer[B] =
    Writer(f(value), log)

  def flatMap[B](f: A => Writer[B]): Writer[B] = {
    val next = f(value)
    Writer(next.value, log ++ next.log)
  }
}

object Writer {
  def pure[A](a: A): Writer[A] = Writer(a, Vector.empty)
}

// State - для изменения состояния библиотеки
case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, newS) = run(s)
      (f(a), newS)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, newS) = run(s)
      f(a).run(newS)
    })
}

object State {
  def pure[S, A](a: A): State[S, A] = State(s => (a, s))
}

// IO - для взаимодействия с пользователем
case class IO[A](unsafeRun: () => A) {
  def map[B](f: A => B): IO[B] =
    IO(() => f(unsafeRun()))

  def flatMap[B](f: A => IO[B]): IO[B] =
    IO(() => f(unsafeRun()).unsafeRun())
}

object IO {
  def pure[A](a: A): IO[A] = IO(() => a)
  def println(line: String): IO[Unit] = IO(() => Predef.println(line))
  def readLine: IO[String] = IO(() => scala.io.StdIn.readLine().trim())
}

// Типы данных
case class Book(title: String, isRare: Boolean)
case class Reader2(name: String)

case class LibraryConfig(
                          maxBooks: Int,
                          loanDays: Int,
                          finePerDay: Double,
                          restrictedBooks: List[String]
                        )

case class LibraryState(
                         catalog: Map[String, Int],
                         borrowed: Map[String, List[String]],
                         currentDay: Int
                       )

object LibraryState {
  def empty: LibraryState = LibraryState(
    catalog = Map(
      "Scala Programming"      -> 3,
      "Functional Programming" -> 2,
      "Clean Code"             -> 1,
      "Rare Ancient Tome"      -> 1
    ),
    borrowed   = Map.empty,
    currentDay = 1
  )
}

// Reader функции - читаем конфигурацию
object LibraryReader {

  def canBorrow(reader: Reader2, state: LibraryState): Reader[LibraryConfig, Boolean] =
    Reader { config =>
      val count = state.borrowed.getOrElse(reader.name, List.empty).length
      count < config.maxBooks
    }

  def dueDate(today: Int): Reader[LibraryConfig, Int] =
    Reader { config => today + config.loanDays }

  def fine(daysLate: Int): Reader[LibraryConfig, Double] =
    Reader { config =>
      if (daysLate <= 0) 0.0
      else daysLate * config.finePerDay
    }

  def isRestricted(book: Book): Reader[LibraryConfig, Boolean] =
    Reader { config =>
      config.restrictedBooks.contains(book.title) || book.isRare
    }
}

// Writer функции - логируем операции
object LibraryWriter {

  def logBorrowDecision(reader: Reader2, book: Book, allowed: Boolean): Writer[Boolean] = {
    val msg =
      if (allowed) s"[ВЫДАЧА] ${reader.name} взял книгу ${book.title}"
      else         s"[ОТКАЗ] ${reader.name} не может взять ${book.title}"
    Writer(allowed, Vector(msg))
  }

  def logDueDate(book: Book, day: Int): Writer[Int] =
    Writer(day, Vector(s"[СРОК] ${book.title} нужно вернуть к дню $day"))

  def logOverdue(reader: Reader2, book: Book, daysLate: Int): Writer[Int] = {
    val msg =
      if (daysLate > 0) s"[ПРОСРОЧКА] ${reader.name} опоздал на $daysLate дней"
      else              s"[ВОВРЕМЯ] ${book.title} возвращена вовремя"
    Writer(daysLate, Vector(msg))
  }

  def logFine(reader: Reader2, amount: Double): Writer[Double] = {
    val msg =
      if (amount > 0) s"[ШТРАФ] ${reader.name} должен $amount руб."
      else            s"[БЕЗ ШТРАФА] ${reader.name} ничего не должен"
    Writer(amount, Vector(msg))
  }
}

// State функции - изменяем состояние
object LibraryState2 {

  def borrowBook(reader: Reader2, book: Book): State[LibraryState, String] =
    State { state =>
      val count = state.catalog.getOrElse(book.title, 0)
      if (count <= 0) {
        (s"Книги '${book.title}' нет в наличии", state)
      } else {
        val newCatalog  = state.catalog.updated(book.title, count - 1)
        val readerBooks = state.borrowed.getOrElse(reader.name, List.empty)
        val newBorrowed = state.borrowed.updated(reader.name, book.title :: readerBooks)
        (s"Книга '${book.title}' выдана '${reader.name}'", state.copy(catalog = newCatalog, borrowed = newBorrowed))
      }
    }

  def returnBook(reader: Reader2, book: Book): State[LibraryState, String] =
    State { state =>
      val readerBooks = state.borrowed.getOrElse(reader.name, List.empty)
      if (!readerBooks.contains(book.title)) {
        (s"У '${reader.name}' нет книги '${book.title}'", state)
      } else {
        val newBorrowed = state.borrowed.updated(reader.name, readerBooks.filterNot(_ == book.title))
        val newCatalog  = state.catalog.updated(book.title, state.catalog.getOrElse(book.title, 0) + 1)
        (s"'${reader.name}' вернул '${book.title}'", state.copy(catalog = newCatalog, borrowed = newBorrowed))
      }
    }

  def nextDay: State[LibraryState, Int] =
    State { state =>
      val newState = state.copy(currentDay = state.currentDay + 1)
      (newState.currentDay, newState)
    }

  def addCopies(book: Book, count: Int): State[LibraryState, String] =
    State { state =>
      val current  = state.catalog.getOrElse(book.title, 0)
      val newState = state.copy(catalog = state.catalog.updated(book.title, current + count))
      (s"Добавлено $count экз. '${book.title}'", newState)
    }
}

// IO сценарий
object LibraryApp {

  val config = LibraryConfig(
    maxBooks        = 3,
    loanDays        = 14,
    finePerDay      = 10.0,
    restrictedBooks = List("Rare Ancient Tome")
  )

  def run: IO[Unit] = for {
    _      <- IO.println("=== Добро пожаловать в библиотеку ===")
    _      <- IO.println("Введите ваше имя:")
    name   <- IO.readLine
    reader  = Reader2(name)
    _      <- IO.println(s"Привет, $name!")
    _      <- IO.println("Выберите команду: borrow или return")
    cmd    <- IO.readLine
    _      <- if (cmd == "borrow") doBorrow(reader)
    else if (cmd == "return") doReturn(reader)
    else IO.println("Неизвестная команда")
    _      <- IO.println("До свидания!")
  } yield ()

  def doBorrow(reader: Reader2): IO[Unit] = for {
    _     <- IO.println("Введите название книги:")
    title <- IO.readLine
    book   = Book(title, isRare = false)
    state  = LibraryState.empty

    canTake    = LibraryReader.canBorrow(reader, state).run(config)
    restricted = LibraryReader.isRestricted(book).run(config)
    decision   = LibraryWriter.logBorrowDecision(reader, book, canTake && !restricted)
    result     = if (decision.value) LibraryState2.borrowBook(reader, book).run(state)
    else ("Выдача невозможна", state)
    due        = LibraryReader.dueDate(state.currentDay).run(config)
    dueLog     = LibraryWriter.logDueDate(book, due)

    _ <- IO.println("\n=== ЧЕК ===")
    _ <- IO.pure(decision.log ++ dueLog.log).flatMap { logs =>
      logs.foldLeft(IO.pure(())) { (acc, line) =>
        acc.flatMap(_ => IO.println(line))
      }
    }
    _ <- IO.println(result._1)
  } yield ()

  def doReturn(reader: Reader2): IO[Unit] = for {
    _      <- IO.println("Введите название книги:")
    title  <- IO.readLine
    book    = Book(title, isRare = false)
    _      <- IO.println("Текущий день:")
    dayStr <- IO.readLine

    currentDay    = dayStr.toIntOption.getOrElse(1)
    daysLate      = currentDay - (1 + config.loanDays)
    overdueLog    = LibraryWriter.logOverdue(reader, book, daysLate)
    fineAmount    = LibraryReader.fine(daysLate).run(config)
    fineLog       = LibraryWriter.logFine(reader, fineAmount)
    state         = LibraryState.empty
    result        = LibraryState2.returnBook(reader, book).run(state)

    _ <- IO.println("\n=== ЧЕК ===")
    _ <- IO.println(result._1)
    _ <- IO.pure(overdueLog.log ++ fineLog.log).flatMap { logs =>
      logs.foldLeft(IO.pure(())) { (acc, line) =>
        acc.flatMap(_ => IO.println(line))
      }
    }
  } yield ()
}

@main def main(): Unit =
  LibraryApp.run.unsafeRun()
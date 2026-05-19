import scala.language.higherKinds

//Блок 1. Tagless Final трейты
// просто описываем что умеет консоль
trait Console[F[_]]:
  def putStrLn(line: String): F[Unit]  // напечатать строку
  def getStrLn: F[String]              // прочитать строку

// описываем что умеет библиотека
trait Library[F[_]]:
  def borrowBook(reader: Reader2, book: Book, state: LibraryState): F[(String, LibraryState)]  // выдать книгу
  def returnBook(reader: Reader2, book: Book, state: LibraryState): F[(Double, LibraryState)]  // вернуть книгу
  def nextDay(state: LibraryState): F[LibraryState]                                            // следующий день
  def showState(state: LibraryState): F[Unit]                                                  // показать состояние

// Блок 2. given instances
// делаем настоящую консоль для IO
given Console[IO] with
  def putStrLn(line: String): IO[Unit] = IO.println(line)  // просто печатаем
  def getStrLn: IO[String] = IO.readLine                   // читаем строку

// делаем настоящую библиотеку для IO
given Library[IO] with
  private val config = LibraryConfig(3, 14, 10.0)  // настройки: 3 книги, 14 дней, штраф 10

  // выдать книгу
  def borrowBook(reader: Reader2, book: Book, state: LibraryState): IO[(String, LibraryState)] =
    val canTake = LibraryReader.canBorrow(reader, state).run(config)  // проверяем лимит
    if canTake then
      val (msg, newState) = LibraryState2.borrowBook(reader, book).run(state)  // меняем состояние
      val due = LibraryReader.dueDate(newState.currentDay).run(config)         // считаем дату возврата
      IO.pure((s"[ВЫДАЧА] ${reader.name} взял '${book.title}', вернуть к дню $due", newState))
    else
      IO.pure((s"[ОТКАЗ] ${reader.name} не может взять '${book.title}'", state))

  // вернуть книгу
  def returnBook(reader: Reader2, book: Book, state: LibraryState): IO[(Double, LibraryState)] =
    val dueDay = 1 + config.loanDays
    val daysLate = if state.currentDay > dueDay then state.currentDay - dueDay else 0  // считаем просрочку
    val fineAmount = LibraryReader.fine(daysLate).run(config)                         // считаем штраф
    val (_, newState) = LibraryState2.returnBook(reader, book).run(state)             // меняем состояние
    IO.pure((fineAmount, newState))

  // следующий день
  def nextDay(state: LibraryState): IO[LibraryState] =
    val (_, newState) = LibraryState2.nextDay.run(state)  // день + 1
    IO.pure(newState)

  // показать состояние (только день и каталог)
  def showState(state: LibraryState): IO[Unit] =
    IO.println(s"День: ${state.currentDay}").flatMap(_ =>
      state.catalog.toList.foldLeft(IO.pure(())) { (acc, entry) =>
        val (title, count) = entry
        acc.flatMap(_ => IO.println(s"  $title: $count экз."))
      }
    )

//  Блок3. extension methods
extension (s: String)
  def toReader: Reader2 = Reader2(s)  // строка -> читатель
  def toBook: Book = Book(s)          // строка -> книга

//  Программа с меню
object LibraryAppTF:

  // главная программа
  def program(using C: Console[IO], L: Library[IO]): IO[Unit] = for
    _ <- C.putStrLn("=== Добро пожаловать в библиотеку (Tagless Final) ===")
    _ <- C.putStrLn("Введите ваше имя:")
    name <- C.getStrLn
    reader = name.toReader
    _ <- C.putStrLn(s"Привет, ${reader.name}!")
    _ <- menu(reader, LibraryState.empty)  // запускаем меню
  yield ()

  // меню с выбором действий
  def menu(reader: Reader2, state: LibraryState)(using C: Console[IO], L: Library[IO]): IO[Unit] = for
    _ <- C.putStrLn("\n=== МЕНЮ ===")
    _ <- C.putStrLn("1. Взять книгу")
    _ <- C.putStrLn("2. Вернуть книгу")
    _ <- C.putStrLn("3. Следующий день")
    _ <- C.putStrLn("4. Показать состояние")
    _ <- C.putStrLn("0. Выход")
    _ <- C.putStrLn("Ваш выбор:")
    choice <- C.getStrLn
    _ <- choice match
      case "1" =>  // взять книгу
        for
          _ <- C.putStrLn("Введите название книги:")
          title <- C.getStrLn
          book = title.toBook
          (msg, newState) <- L.borrowBook(reader, book, state)
          _ <- C.putStrLn(msg)
          _ <- menu(reader, newState)  // возвращаемся в меню с новым состоянием
        yield ()
      case "2" =>  // вернуть книгу
        for
          _ <- C.putStrLn("Введите название книги:")
          title <- C.getStrLn
          book = title.toBook
          (fineAmount, newState) <- L.returnBook(reader, book, state)
          _ <- if fineAmount > 0 then C.putStrLn(s"[ШТРАФ] $fineAmount руб.") else C.putStrLn("[БЕЗ ШТРАФА]")
          _ <- menu(reader, newState)
        yield ()
      case "3" =>  // следующий день
        for
          newState <- L.nextDay(state)
          _ <- C.putStrLn(s"День ${newState.currentDay}")
          _ <- menu(reader, newState)
        yield ()
      case "4" =>  // показать состояние
        for
          _ <- L.showState(state)
          _ <- menu(reader, state)  // состояние не меняется
        yield ()
      case "0" => C.putStrLn("До свидания!")  // выход
      case _   =>  // неизвестная команда
        for
          _ <- C.putStrLn("Неизвестная команда")
          _ <- menu(reader, state)
        yield ()
  yield ()


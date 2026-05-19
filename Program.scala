// Блок 5. IO сценарий 

object LibraryApp:

  // настройки
  val config = LibraryConfig(
    maxBooks = 3,
    loanDays = 14,
    finePerDay = 10.0
  )

  // точка входа
  def run: IO[Unit] = for
    _ <- IO.println("=== Добро пожаловать в библиотеку ===")
    _ <- IO.println("Введите ваше имя:")
    name <- IO.readLine
    reader = Reader2(name)
    _ <- IO.println(s"Привет, $name!")
    _ <- menu(reader, LibraryState.empty)
  yield ()

  // простое меню 
  def menu(reader: Reader2, state: LibraryState): IO[Unit] = for
    _ <- IO.println("\n=== МЕНЮ ===")
    _ <- IO.println("1. Взять книгу")
    _ <- IO.println("2. Вернуть книгу")
    _ <- IO.println("3. Следующий день")
    _ <- IO.println("4. Показать состояние")
    _ <- IO.println("0. Выход")
    _ <- IO.println("Ваш выбор:")
    choice <- IO.readLine
    result <- choice match
      case "1" => borrowBook(reader, state).flatMap(newState => menu(reader, newState))
      case "2" => returnBook(reader, state).flatMap(newState => menu(reader, newState))
      case "3" => nextDay(state).flatMap(newState => menu(reader, newState))
      case "4" => showState(state).flatMap(_ => menu(reader, state))
      case "0" => IO.println("До свидания!")
      case _   => IO.println("Неизвестная команда").flatMap(_ => menu(reader, state))
  yield result

  // выдать книгу
  def borrowBook(reader: Reader2, state: LibraryState): IO[LibraryState] = for
    _ <- IO.println("Введите название книги:")
    title <- IO.readLine
    book = Book(title)

    // проверяем лимит
    canTake = LibraryReader.canBorrow(reader, state).run(config)

    // печатаем чек
    _ <- IO.println("\n=== ЧЕК ВЫДАЧИ ===")
    _ <- if canTake then
      IO.println(s"[ВЫДАЧА] ${reader.name} взял книгу ${book.title}")
    else
      IO.println(s"[ОТКАЗ] ${reader.name} не может взять ${book.title}")

    // если можно - выдаём
    result <- if canTake then
      val (msg, newState) = LibraryState2.borrowBook(reader, book).run(state)
      val due = LibraryReader.dueDate(newState.currentDay).run(config)
      for
        _ <- IO.println(s"[СРОК] ${book.title} нужно вернуть к дню $due")
        _ <- IO.println(msg)
      yield newState
    else
      IO.println("Выдача невозможна").map(_ => state)
  yield result

  // вернуть книгу
  def returnBook(reader: Reader2, state: LibraryState): IO[LibraryState] = for
    _ <- IO.println("Введите название книги:")
    title <- IO.readLine
    book = Book(title)

    _ <- IO.println("\n=== ЧЕК ВОЗВРАТА ===")

    // считаем просрочку (книга взята в день 1)
    dueDay = 1 + config.loanDays
    currentDay = state.currentDay
    daysLate = if currentDay > dueDay then currentDay - dueDay else 0

    _ <- if daysLate > 0 then
      IO.println(s"[ПРОСРОЧКА] ${reader.name} опоздал на $daysLate дней")
    else
      IO.println(s"[ВОВРЕМЯ] ${book.title} возвращена вовремя")

    // считаем штраф
    fineAmount = if daysLate > 0 then daysLate * config.finePerDay else 0.0

    _ <- if fineAmount > 0 then
      IO.println(s"[ШТРАФ] ${reader.name} должен $fineAmount руб.")
    else
      IO.println(s"[БЕЗ ШТРАФА] ${reader.name} ничего не должен")

    // возвращаем книгу
    (msg, newState) = LibraryState2.returnBook(reader, book).run(state)
    _ <- IO.println(msg)
    _ <- if fineAmount > 0 then
      IO.println(s"Штраф к оплате: $fineAmount руб.")
    else
      IO.pure(())
  yield newState

  // следующий день
  def nextDay(state: LibraryState): IO[LibraryState] = for
    (msg, newState) = LibraryState2.nextDay.run(state)
    _ <- IO.println(s"День ${newState.currentDay}")
  yield newState

  // показать состояние
  def showState(state: LibraryState): IO[Unit] = for
    _ <- IO.println("\n=== СОСТОЯНИЕ БИБЛИОТЕКИ ===")
    _ <- IO.println(s"Текущий день: ${state.currentDay}")
    _ <- IO.println("Каталог:")
    _ <- printCatalog(state.catalog.toList)
    _ <- IO.println("Выданные книги:")
    _ <- printBorrowed(state.borrowed.toList)
  yield ()

  // рекурсивно печатаем каталог
  def printCatalog(list: List[(String, Int)]): IO[Unit] = list match
    case Nil => IO.pure(())
    case (title, count) :: rest =>
      IO.println(s"  $title: $count экз.").flatMap(_ => printCatalog(rest))

  // рекурсивно печатаем выданные 
  def printBorrowed(list: List[(String, List[String])]): IO[Unit] = list match
    case Nil => IO.pure(())
    case (reader, books) :: rest =>
      IO.println(s"  $reader: ${books.mkString(", ")}").flatMap(_ => printBorrowed(rest))

  
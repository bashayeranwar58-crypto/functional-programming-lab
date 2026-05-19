// Блок 2. Reader функции - работа с конфигом

object LibraryReader:

  // можно ли взять книгу? (проверка лимита)
  def canBorrow(reader: Reader2, state: LibraryState): Reader[LibraryConfig, Boolean] =
    Reader { config =>
      val count = state.borrowed.getOrElse(reader.name, List.empty).length
      count < config.maxBooks
    }

  // дата возврата
  def dueDate(today: Int): Reader[LibraryConfig, Int] =
    Reader { config => today + config.loanDays }

  // штраф
  def fine(daysLate: Int): Reader[LibraryConfig, Double] =
    Reader { config =>
      if daysLate <= 0 then 0.0
      else daysLate * config.finePerDay
    }

// Блок 3. Writer функции - логирование

object LibraryWriter:

  // лог о выдаче
  def logBorrowDecision(reader: Reader2, book: Book, allowed: Boolean): Writer[Boolean] =
    val msg =
      if allowed then s"[ВЫДАЧА] ${reader.name} взял книгу ${book.title}"
      else s"[ОТКАЗ] ${reader.name} не может взять ${book.title}"
    Writer(allowed, Vector(msg))

  // лог о сроке возврата
  def logDueDate(book: Book, day: Int): Writer[Int] =
    Writer(day, Vector(s"[СРОК] ${book.title} нужно вернуть к дню $day"))

  // лог о просрочке
  def logOverdue(reader: Reader2, book: Book, daysLate: Int): Writer[Int] =
    val msg =
      if daysLate > 0 then s"[ПРОСРОЧКА] ${reader.name} опоздал на $daysLate дней"
      else s"[ВОВРЕМЯ] ${book.title} возвращена вовремя"
    Writer(daysLate, Vector(msg))

  // лог о штрафе
  def logFine(reader: Reader2, amount: Double): Writer[Double] =
    val msg =
      if amount > 0 then s"[ШТРАФ] ${reader.name} должен $amount руб."
      else s"[БЕЗ ШТРАФА] ${reader.name} ничего не должен"
    Writer(amount, Vector(msg))

// Блок 4. State функции - изменение состояния

object LibraryState2:

  // выдать книгу
  def borrowBook(reader: Reader2, book: Book): State[LibraryState, String] =
    State { state =>
      val count = state.catalog.getOrElse(book.title, 0)
      if count <= 0 then
        (s"Книги '${book.title}' нет в наличии", state)
      else
        val newCatalog = state.catalog.updated(book.title, count - 1)
        val readerBooks = state.borrowed.getOrElse(reader.name, List.empty)
        val newBorrowed = state.borrowed.updated(reader.name, book.title :: readerBooks)
        (s"Книга '${book.title}' выдана '${reader.name}'", state.copy(catalog = newCatalog, borrowed = newBorrowed))
    }

  // вернуть книгу
  def returnBook(reader: Reader2, book: Book): State[LibraryState, String] =
    State { state =>
      val readerBooks = state.borrowed.getOrElse(reader.name, List.empty)
      if !readerBooks.contains(book.title) then
        (s"У '${reader.name}' нет книги '${book.title}'", state)
      else
        val newBorrowed = state.borrowed.updated(reader.name, readerBooks.filterNot(_ == book.title))
        val newCatalog = state.catalog.updated(book.title, state.catalog.getOrElse(book.title, 0) + 1)
        (s"'${reader.name}' вернул '${book.title}'", state.copy(catalog = newCatalog, borrowed = newBorrowed))
    }

  // следующий день
  def nextDay: State[LibraryState, Int] =
    State { state =>
      val newState = state.copy(currentDay = state.currentDay + 1)
      (newState.currentDay, newState)
    }
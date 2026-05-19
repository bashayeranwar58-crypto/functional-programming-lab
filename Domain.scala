// Блок 1. Предметные типы

// книга - только название (редкое убрали по просьбе препода)
case class Book(title: String)

// читатель - просто имя
case class Reader2(name: String)

// конфиг - настройки библиотеки
case class LibraryConfig(
                          maxBooks: Int,     // сколько книг можно взять
                          loanDays: Int,     // срок выдачи
                          finePerDay: Double // штраф за день просрочки
                        )

// состояние библиотеки
case class LibraryState(
                         catalog: Map[String, Int],        // книга -> сколько экземпляров
                         borrowed: Map[String, List[String]], // читатель -> список книг
                         currentDay: Int                   // текущий день
                       )

// начальное состояние
object LibraryState:
  def empty: LibraryState = LibraryState(
    catalog = Map(
      "Scala Programming"      -> 3,
      "Functional Programming" -> 2,
      "Clean Code"             -> 1
    ),
    borrowed   = Map.empty,
    currentDay = 1
  )
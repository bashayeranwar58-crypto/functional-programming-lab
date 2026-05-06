Лабораторная работа 1 — Библиотека
Вариант 2
Описание:
Система управления библиотекой: выдача книг, возврат и подсчёт штрафов.

Структура проекта
Library.scala
├── Блок 0 — Инфраструктура (Monad, Reader, Writer, State, IO)
├── Блок 1 — Предметные типы (Book, Reader2, LibraryConfig,     LibraryState)
├── Блок 2 — Reader функции (canBorrow, dueDate, fine, isRestricted)
├── Блок 3 — Writer функции (logBorrowDecision, logDueDate, logOverdue, logFine)
├── Блок 4 — State функции (borrowBook, returnBook, nextDay, addCopies)
└── Блок 5 — IO сценарий (LibraryApp)

Как запустить
Требования
·	Scala 3.8.3
·	Intelij IDEA
Запуск через
Intelij IDEA
Запуск напрямую
Run main.Scala

Использование
После запуска программа спросит:
=== Добро пожаловать в библиотеку ===
Введите ваше имя:
> Bashaer

Привет, Bashaer!
Выберите команду: borrow или return
> borrow

Введите название книги:
> Scala Programming

=== ЧЕК ===
[ВЫДАЧА] Bashaer взял книгу Scala Programming
Книга 'Scala Programming' выдана 'Bashaer'
[СРОК] Scala Programming нужно вернуть к дню 15


Доступные книги
·	Scala Programming (3 экз.)
·	Functional Programming (2 экз.)
·	Clean Code (1 экз.)
·	Rare Ancient Tome — запрещена к выдаче
Параметр	Значение
Максимум книг на читателя	3
Срок выдачи	14 дней
Штраф за день	10 руб.
Конфигурация


Лабораторная работа 1 — Библиотека 



Вариант 2

Описание:

Система управления библиотекой: выдача книг, возврат и подсчёт штрафов.

Реализация  использованием:

- Reader для конфигурации

- Writer для логирования

- State для состояния

-IO для ввода/вывода

Структура проекта

src

Monad.scala - Блок 0 — Инфраструктура (Monad, Reader, Writer, State, IO)

Domain.scala - Блок 1 — Предметные типы (Book, Reader2, LibraryConfig, LibraryState)

Logic.scala - Блок 2-4 — Reader, Writer, State функции

Program.scala -Блок 5 — IO сценарий (LibraryApp)

main

&#x20;

Конфигурация библиотеки

- Максимум книг на читателя: 3

- Срок выдачи: 14 дней

- Штраф за день просрочки: 10 руб.

Доступные книги (после удаления редких)

- Scala Programming — 3 экз.

- Functional Programming — 2 экз.

- Clean Code — 1 экз.



Меню программы

После запуска программа покажет меню:

=== МЕНЮ ===



Взять книгу



Вернуть книгу



Следующий день



Показать состояние



Выход





Пример работы:

=== Добро пожаловать в библиотеку ===

Введите ваше имя:



Bashaer

Привет, Bashaer!



=== МЕНЮ ===



Взять книгу



Вернуть книгу



Следующий день



Показать состояние



Выход

Ваш выбор:



1

Введите название книги:

Scala Programming



=== ЧЕК ВЫДАЧИ ===

[ВЫДАЧА] Bashaerr взял книгу Scala Programming

[СРОК] Scala Programming нужно вернуть к дню 15

Книга 'Scala Programming' выдана 'Bashaer'


Что реализовано

Блок	Функции

Reader	canBorrow, dueDate, fine

Writer	logBorrowDecision, logDueDate, logOverdue, logFine

State	borrowBook, returnBook, nextDay

IO	консольное меню, взаимодействие с пользователем








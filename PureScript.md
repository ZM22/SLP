  PureScript-это строго типизированный, чисто функциональный язык программирования, который компилируется в JavaScript. 
Он может быть использован для разработки веб-приложений, серверных приложений, а также настольных приложений с использованием Electron. 
Его синтаксис в основном сопоставим с синтаксисом Haskell. Кроме того, он вводит полиморфизм строк и расширяемые записи. 
Кроме того, в отличие от Haskell, PureScript придерживается строгой стратегии оценки.

  PureScript был первоначально разработан Филом Фрименом в 2013 году.
Он начал работать над PureScript, поскольку различные попытки скомпилировать Haskell в JavaScript с сохранением его семантики 
(например, с помощью Fay, Haste или GHCJS) не удовлетворили его.

  С тех пор он был подхвачен сообществом и разработан на GitHub.  
Дополнительные основные инструменты, разработанные сообществом, включают специальный инструмент сборки "Pulp", каталог документации "Pursuit", и менеджер пакетов "Spago".

  PureScript имеет строгую оценку, постоянные структуры данных и вывод типов. Система типов PureScript имеет много общих черт с аналогичными функциональными языками , 
такими как Haskell: алгебраические типы данных и сопоставление шаблонов, типы более высокого рода, классы типов и функциональные зависимости, 
а также полиморфизм более высокого ранга. Система типов PureScript добавляет поддержку полиморфизма строк и расширяемых записей. 
Однако в PureScript отсутствует поддержка некоторых более продвинутых функций Haskell, таких как GADTs и семейства типов.

  Компилятор PureScript пытается создать читаемый код JavaScript, где это возможно.     
Благодаря простому интерфейсу FFIон также позволяет повторно использовать существующий код JavaScript. 

  PureScript поддерживает инкрементную компиляцию, а дистрибутив компилятора включает в себя поддержку создания плагинов редактора исходного кода для итеративной разработки. 
Плагины редактора существуют для многих популярных текстовых редакторов, включая Vim, Emacs, Sublime Text, Atom и Visual Studio Code.

  PureScript поддерживает типизированную разработку с помощью функции типизированных отверстий, в которой программа может быть построена с отсутствующими подвыражениями. 
Компилятор впоследствии попытается определить типы отсутствующих подвыражений и сообщить об этих типах пользователю. 
Эта функция вдохновила аналогичную работу в компиляторе GHC Haskell.

  Вот минимальная программа "Hello world" на языке PureScript:
  ```PureScript
  модуль Main where

  import Effect.Console (log)

  main = log "Hello World!"
  ```
  Здесь тип программы выводится и проверяется компилятором PureScript. Более подробная версия той же программы может включать явные аннотации типов:
  ```PureScript
  модуль Main , где

  import Prelude

  import Effect (Эффект)
  import Effect.Console (log)


  main :: Effect Unit
  main = log "Hello World!"
  ```

# Russian xmaxima translation by S. Semerikov and V. Zhytnikov


### Menu.tcl
mcset ru "Edit" "Редактировать"
mcset ru "Save to a file" "Сохранить в файле"
mcset ru "Error writing to file" "Ошибка записи в файл"
mcset ru "Batch File" "Выполнить пакетный файл"
mcset ru "Open a file to Batch" "Открыть пакетный файл"
mcset ru "Error writing to file:\n%s" "Ошибка записи в файл:\n%s"
mcset ru "File" "Файл"
mcset ru "Batch File Silently" "Выполнить пакетный файл без вывода"
mcset ru "Open a file to BATCHLOAD" "Открыть файл с помощью BATCHLOAD"
mcset ru "Save Expressions to File" "Сохранить выражения в файле"
mcset ru "Interrupt" "Прервать"
mcset ru "Restart" "Перезапустить"
mcset ru "Exit" "Выход"
mcset ru "Edit" "Правка"
mcset ru "Cut" "Вырезать"
mcset ru "Copy" "Копировать"
mcset ru "Paste" "Вставить"
mcset ru "Kill" "Отменить"
mcset ru "Yank" "Повторить"
mcset ru "Previous Input" "Предыдущий ввод"
mcset ru "Next Input" "Следующий ввод"
mcset ru "Clear input" "Очистить ввод"
mcset ru "Save Console to File" "Сохранить консоль в файле"
mcset ru "Options" "Настройки"
mcset ru "Toggle Browser Visibility" "Переключить видемость браузера"
mcset ru "Plot Windows" "Графическое окно"
mcset ru "Embedded" "Встроенное"
mcset ru "Separate" "Отдельное"
mcset ru "Multiple" "Множественное"
mcset ru "Preferences" "Настройки"
mcset ru "Show Tcl Console" "Отобразить консоль Tcl"
mcset ru "Clear Memory" "Очистить память"
mcset ru "Kill All" "Очистить всё"
mcset ru "Run Tests" "Запуск тестов"
mcset ru "Help" "Справка"
mcset ru "Maxima Help" "Справка по Maxima"
mcset ru "Maxima Homepage" "Домашняя страница Maxima"
mcset ru "Project Page" "Страница проекта"
mcset ru "Bug Reports" "Для сообщений об ошибках"


### EHref.tcl
mcset ru "args to href.  Wanted 1, got:" "аргументов в href.  Ожидается 1, получено:"


### EMaxima.tcl
mcset ru "unable to call" "невозможно вызвать"


### EOpenplot.tcl
mcset ru "not allowed, only" "недопустим, только"


### Bindings.tcl
mcset ru "
Bindings:
<Return>   This sends the current expression (ie where the insert
cursor is)  for evaluation.
<Linefeed> (Control-j) This inserts a newline, and is useful
for entering multiline input.
<Control-k> Kills the current line and puts it in kill ring.
Successive control-k's append their output together.
<Control-y> Yank out the last kill, Meta-y cycles thru previous
kills.
<Control-g> Interrupt the current computation.
<Alt-p>   Previous input, or if repeated cycle through the previous
inputs.  If the current input is not empty, then
match only inputs which begin with the current input.
<Alt-n>   Like Previous input, but in opposite direction.
" "
Клавиатурные привязки:
<Return>   Вычисляет текущее выражение (находящееся в позиции
курсора).
<Linefeed> (Control-j) Вставляет новую строку; полезно при
многострочном вводе.
<Control-c> Копирует выделенный фрагмент в буфер.
<Control-x> Удаляет выделенный фрагмент в буфер.
<Control-v> Вставляет фрагмент из буфер.
<Control-u> Удаляет строку, добавляя ее в буфер удаленных строк.
<Control-k> Удаляет строку, начиная с текущей позиции, добавляя
ее в буфер удаленных строк. Последующие нажатия Control-k's
соединяют их вместе.
<Control-y> Возвращает последнюю удаленную строку, Meta-y 
циклически перебирает предшествующие удаленные строки.
<Control-g> Прерывает текущие вычисления.
<Alt-p>   Предыдущий ввод, при повторном нажатии выводит
все предшествующие строки. Если текущая строка ввода не пуста,
выводит только те строки, начало которых совпадает с текущей
строкой.
<Alt-n>   То же, что и предыдущий ввод, но в обратном направлении.
"


### Browser.tcl
mcset ru "illegal command in rich text:" "Неверная команда в Rich Text:"
mcset ru "unable to evaluate command:" "Невозможно выполнить команду:"
mcset ru "History List" "История посещений"
mcset ru "This window may be dragged elsewhere by grabbing this title bar with the mouse.  Double clicking on a history item, moves to that page." \
         "Это окно может быть перемещено в любое место с помощью захвата мышью его заголовка.   Двойной щелчок на элементе истории приводит к переходу на заданную страницу."
mcset ru "Close" "Закрыть"
mcset ru "Remove the history list" "Удалить историю посещений"
mcset ru "unknown type:" "unknown неизвестный тип:"
mcset ru "error unsupported url type:" "error неподдерживаемый тип url:"
mcset ru "Error connecting to %s on %s\n%s" "Ошибка соединения с %s на порту %s\n%s"
mcset ru "not supported" "не поддерживается"
mcset ru "need version of tk with 'binary' command for images" \
         "Необходима версия Tk с командой 'binary' для изображений"
mcset ru "could not get image" "невозможно получить изображение"
mcset ru "unknown type of image" "неизвестный тип изображения"
mcset ru "cant remove tmp file" "невозможно удалить временный файл"
mcset ru "Unable to open an image for" "Невозможно открыть изображение для"
mcset ru "Opening" "Открываем"
mcset ru "unable to evaluate" "невозможно вычислить"
mcset ru "Font Settings\nThe proportional font is " "Установки шрифтов\nПропорциональный шрифт "
mcset ru "with a size adjustment of " " с корректировкой размера "
mcset ru "\nThe fixed font is " "\nМоноширинный шрифт "
mcset ru "Default nmtp servers  " "nmtp-сервер по умолчанию "
mcset ru "http Proxy host and port:" "http proxy-сервер и порт:"
mcset ru "\nIf you are behind a firewall enter the name of your http proxy host and port,\n eg: `foo.ma.utexas.edu 3128', otherwise leave this blank" \
         "\nЕсли Вы находитесь за брандмауэром, введите имя Вашего http proxy-сервера и порт,\n например: `foo.ma.utexas.edu 3128', в противном случае оставьте это поле пустым"
mcset ru "\nShould plot windows be " "\nГрафическое окно должно быть " 
mcset ru " Apply and Quit " " Применить и выйти "
mcset ru " Apply " " Применить "
mcset ru " Cancel " " Отменить "
mcset ru " Save Preferences " " Сохранить установки "


### Getdata1.tcl
mcset ru "error:" "ошибка:"
mcset ru "writing" "записывается"


### Myhtml.tcl
mcset ru "Internal font error:" "Внутренняя ошибка шрифта:"
mcset ru "bad window" "неверное окно"
mcset ru "not yet" "ещё не"
mcset ru "Click to follow link to" "Нажмите для перехода на ссылку"
mcset ru " Enter search keywords: " " Введите слова для поиска: "
mcset ru "cant decode hex" "невозможно декодировать шестнадцатиричное"


### OpenMath.tcl
mcset ru "Back" "Назад"
mcset ru "Forward" "Вперёд"
mcset ru "Edit" "Редактировать"
mcset ru "Bring down a menu with some edit options" "Выпадающее меню с настройками редактирования"
mcset ru "show edit bar" "показать панель редактирования"
mcset ru "Mark the currently selected region of the text window, so that clicking on the region will cause evaluation by a program specified below." \
         "Помечает текущую выделенную область в текстовом окне, чтобы при otkxrt она был вычислена выбранной ниже программой."
mcset ru "mark for eval" "выделить для вычисления"
mcset ru "Set `%s' to be the current type used by 'mark for eval' for making the selected region sensitive to double clicking." \
         "Устанавливает `%s' в качестве текущего типа, используемого 'выделить для вычисления' для установки выделенного участка чувствительным к двойному щелчку."
mcset ru "Options" "Настройки"
mcset ru "File" "Файл"
mcset ru "Offer possible help options, including toggling \n	whether to show balloon help messages" \
         "Установки справки, в т.ч. -\n включение/выключение всплывающих подсказок"
mcset ru "Hide Balloon Help" "Спрятать всплывающие подсказки"
mcset ru "Show Balloon Help" "Показывать всплывающие подсказки"
mcset ru {Menu of file options, for saving and preferences} {Файловое меню - сохранение и настройки}
mcset ru "Reload" "Перезагрузить"
mcset ru "Interrupt" "Прервать"
mcset ru {Try to interrupt the current remote computations.} {Попытаться прервать текущие удаленные вычисления.}
mcset ru "Abort" "Отменить"
mcset ru {Try to abort the current remote computation.} {Попытаться отменить текущие удаленные вычисления.}
mcset ru "Stop" "Остановить"
mcset ru {Stop reading the current url or image.} {Остановить чтение текущего URL или изображения.}
mcset ru "Forget" "Забыть"
mcset ru {Move back one in the history, and remove the current one from the history, unless it was the first window.} \
         {Возврат на предыдущий элемент истории с удалением текущего, если он не первый.}
mcset ru "History" "История"
mcset ru {Display the history list, so that one may be selected by clicking.} \
         {Отображает историю с выбором элемента по нажатию.}
mcset ru "Base Program" "Главная программа"
mcset ru {Show and allow altering of the base program, which shows which is the default host for programs to run on.   May also be specified in <body baseprogram= ...> in the .html file.} \
         {Показывает и позволяет изменять главную программу, которая указывается узлом по умолчанию для работающих на нем программ. Также может быть указана тегом <body baseprogram= ...> в HTML-файле.}
mcset ru "Save to file" "Сохранить в файле"
mcset ru "Fonts" "Шрифты"
mcset ru {set the default font sizes and types} {Устанавливает типы и размеры шрифтов по умолчанию}
mcset ru "Exit" "Выход"
mcset ru {Exit this program} {Выход из программы}
mcset ru \
{Fetch the URL or FILE indicated in the entry box. \
				   A local file is something like file:/home/wfs/foo.om, and a URL \
				   begins with http.} \
{Выбирает URL или файл, указанный в строке ввода. \
				   Локальный файл задается как file:/home/wfs/foo.om, а URL \
				   начинается с http.}

mcset ru {Address of the current document.  You may modify it and type Enter, to fetch a new document.} \
         {Адрес текущего документа. Чтобы выбрать новый документ, введите адрес и нажмите Enter.}
mcset ru "Could not open file %s\n%s" "Невозможно открыть файл %s\n%s"
mcset ru "wrote" "записан"
mcset ru "Double clicking (with the left mouse button), in the marked region will cause evaluation. " \
         "Двойной щелчок левой кнопкой мыши в помеченной области выполнит вычисление. "
mcset ru "No result field with" "Отсутствует поле результата с"
mcset ru "Failed to eval region" "Ошибка вычисления области"
mcset ru "Reload the current URL displayed in the window." "Перезагрузка URL, отображенного в окне."


### Proxy.tcl
mcset ru "Could not connect" "Невозможно соединиться с"


### Wmenu.tcl
mcset ru "needs a window name arg" "аргументом требуется имя окна"
mcset ru "error" "ошибка"
mcset ru "must be one of:" "требуется одно из:"
mcset ru "This is a menu window" "Меню окна"


### Tryembed.tcl
mcset ru "unknown host:" "неизвестный хост:"
mcset ru "connection from %s and %s disallowed" "соединение с %s и %s запрещено"
mcset ru "bad port:" "неверный порт:"
mcset ru "wrong args: socket host port OR socket -server command port" \
	 "неверные аргументы: socket host port ИЛИ socket -server command port"
mcset ru "Cannot change" "Невозможно изменить"
mcset ru "unknown option" "неизвестная опция"


### Tkmaxima.tcl
mcset ru "Usage: xmaxima \[options\] \[url\]" "Использование: xmaxima \[опции\] \[url\]"
mcset ru "           If given, \[url\] will be opened in the help browser instead" "           Если задан \[url\], он будет открыт в окне браузера вместо"
mcset ru "           of the default starting page." "           начальной страницы по умолчанию."
mcset ru "options:" "опции:"
mcset ru "    --help: Display this usage message." "    --help: Отображает это сообщение об использовании."
mcset ru "    -l <lisp>, --lisp=<lisp>: Use lisp implementation <lisp>." "    -l <lisp>, --lisp=<lisp>: Использовать реализацию Lisp <lisp>."
mcset ru "    --use-version=<version>: Launch maxima version <version>." "    --use-version=<version>: Запустить Maxima версии <version>."
mcset ru "Error: arguments %s not understood." "Ошибка: неопознанные аргументы: %s"
mcset ru "Error sourcing %s\n%s" "Ошибка при чтении %s\n%s"
mcset ru "Error starting Maxima:" "Ошибка при запуске Maxima:"


### Send-some.tcl
mcset ru "connection closed" "соединение закрыто"
mcset ru "sending" "отсылка"
mcset ru "on" "на"
mcset ru "had error in" "обнаружена ошибка в"
mcset ru "connecting to" "соединение с"
mcset ru "connected to" "соединено с"
mcset ru "Sending scoket interrupt" "Отправка прерывания сокету"
mcset ru "connecting" "соединение с"
mcset ru "Can't connect to" "Невозможно установить соединение с"
mcset ru "You can try another host by altering Base Program under the \"File\" menu." \
         "Попытайтесь выбрать другой хост в пункте \"Главная программа\" меню \"Файл\"."
	 

### RunMaxima.tcl	 
mcset ru "Could not start Maxima - empty command" "Запуск Maxima невозможен - пустая команда"
mcset ru "Can't execute" "Невозможно выполнить"
mcset ru "Could not open a socket " "Невозможно открыть сокет "
mcset ru "Closed socket" "Закрытый сокет"
mcset ru "Killed process" "Завершенный процесс"
mcset ru "Starting Maxima" "Запуск Maxima"
mcset ru "Starting maxima timed out.  Wait longer?" "Истекло время ожидания запуска Maxima. Подождать еще?"
mcset ru "Starting Maxima timed out" "Истекло время ожидания запуска Maxima"
mcset ru "Failed to start Maxima" "Запуск Maxima невозможен"
mcset ru "Started Maxima" "Maxima запущена"
mcset ru "Error sending to Maxima:" "Ошибка пересылки в Maxima:"
mcset ru "You must Restart" "Необходимо Перезапустить"
mcset ru "You may need to Restart" "Возможно необходимо Перезапустить"
mcset ru "socket closed" "сокет закрыт"
mcset ru "timed out" "таймаут"
mcset ru "Sending signal %s to process %s" "Посылаем сигнал %s процессу %s"



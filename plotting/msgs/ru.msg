# Russian omplotdata translation by S. Semerikov and V. Zhytnikov


##### Constants.tcl
mcset ru "A URL (universal resource locator) such as http://www.ma.utexas.edu/foo.om" \
         "URL (универсальный локатор ресурса), такой как http://www.ma.utexas.edu/foo.om"
mcset ru "A string to search for, to get an initial position" \
         "Строка поиска для получения начальной позиции"
mcset ru "A regexp to search for, to get an initial position" \
         "Регулярное выражение для поиска начальной позиции"


##### Parse.tcl
mcset ru "parser unrecognized:" "parser не распознал:"
mcset ru \
{
    The syntax is like C except that it is permitted to write x^n
    instead of pow(x,n).
} \
{
     Синтаксис похож на C, за исключением того, что вместо pow(x,n)
     записывается x^n.
}
mcset ru "Functions:" "Функции:"
mcset ru "Operators:" "Операторы:"
mcset ru "syntax error" "синтаксическая ошибка"


##### NPlot3d.tcl
mcset ru "from %s to %s requires at least 2 points" "от %s до %s требуется минимум 2 точки"
mcset ru "mismatch" "несоответствие"


##### Getopt.tcl
mcset ru "error" "ошибка"
mcset ru "does not take the key" "не принимает ключ"
mcset ru "is required option for" "требует опцию для"


##### Printops.tcl
mcset ru "Printer Setup" "Установки печати"
mcset ru "Save via ftp" "Сохранить по ftp"
mcset ru "Save as Postscript File" "Сохранить как Postscript-файл"
mcset ru "Print To Printer" "Печатать на принтер"
mcset ru "Center on Page" "Выровнять по центру"
mcset ru "Landscape Mode" "Альбомная ориентация"
mcset ru "do margin" "делать отступ"


##### Paths.tcl
mcset ru "Maxima data directory not found in '%s'" "Каталог с данными (data) Maxima не найден в '%s'"
mcset ru "Documentation not found in '%s'" "Документация не найдена в '%s'"
mcset ru "Starting documentation not found in '%s'" "Введная документация не найдена в '%s'"
mcset ru "Error: Maxima executable not found\n%s\n\n Try setting the environment variable  XMAXIMA_MAXIMA." \
         "Ошибка: исполняемый файл Maxima не найден\n%s\n\n Попытайтесь установить переменную окружения XMAXIMA_MAXIMA."
mcset ru "Error: maxima executable not found." "Ошибка: Исполняемый файл maxima не найден."


##### Plotdf.tcl
mcset ru "A direction field plotter by William Schelter" "Построитель поля направлений Вильяма Шелтера"
mcset ru "Direction Fields" "Поля направлений"
mcset ru "Integrate" "Интегрировать"
mcset ru {Causes clicking on the  plot with the left mouse button at a point, to draw a trajectory passing through that point.   Under Config there is an entry box which allows entering exact x,y coordinates, and which also records the place of the last trajectory computed.} \
         {Нажатие на графике левой кнопкой мыши приводит к рисованию траектории, проходящую через данную точку. В меню Настройка есть поля ввода, позволяющие ввести точные координаты x, y, и указать местоположение последней вычисленной траектории.}
mcset ru "Plot Versus t" "График от t"
mcset ru "Plot the x and y values for the  last trajectory versus t." "Рисует траектории x(t) и y(t)"
mcset ru "X and Y versus t" "X и Y от t"
mcset ru "bad -ode argument:" "неверный параметр -ode :"
mcset ru "\nwant d(y,x)=f(x,y) \n   OR d(x,t)=f(x,y) d(y,t)=g(x,y)" "\nожидается d(y,x)=f(x,y) \n   или d(x,t)=f(x,y) d(y,t)=g(x,y)"
mcset ru \
			   {
			       William Schelter's solver/plotter for ode systems.

			       To QUIT this HELP click here.

			       Clicking at a point computes the trajectory
			       (x(t),y(t)) starting at that point, and satisfying
			       the differential equation
			
			       dx/dt = dxdt
			       dy/dt = dydt

			       By clicking on Zoom, the mouse now allows you to zoom
			       in on a region of the plot.  Each click near a point
			       magnifies the plot, keeping the center at the point
			       you clicked.  Depressing the SHIFT key while clicking
			       zooms in the opposite direction.

			       To resume computing trajectories click on Integrate.

			       To change the differential equation, click on Config and
			       enter new values in the entry windows, and then click on
			       Replot in the main menu bar.

			       Holding the right mouse button down allows you to drag
			       (translate) the plot sideways or up and down.

			       Additional parameters such as the number of steps (nsteps),
			       the initial t value (tinitial), and the x and y centers
			       and radii, may be set under the  Config menu.

			       You may print to a postscript printer, or save the plot 
				   as a postscript file, by clicking on save.   To change 
				   between printing and saving see the Print Options under Config.
			
			   } \
                           {
 			       Вычислитель/построитель Вильяма Шелтера для систем ОДУ.

 			       Для выхода из этой подсказки нажмите здесь.

 			       Нажатие в точке вычисляет траектория (x(t),y(t)),
 			       начинающуюся с этой точки, и удовлетворяющей
 			       дифференциальному уравнению

			       dx/dt = dxdt
			       dy/dt = dydt

 			       При нажатии на Масштабировать, можно масштабировать 
 			       мышью нарисованный участок. Каждый щелчок мышью
 			       увеличивает изображение, сохраняя центр в точке
 			       нажатия. Удержание клавиши SHIFT в момент щелчка
 			       уменьшает изображение.
 
 			       Для продолжения вычисления выберите Интегрировать.
 
 			       Для изменения дифференциальных уравнений выберите
 			       Настройка и введите новые значения в поля ввода, после
 			       чего выберите Перерисовать в главном меню.
 
 			       Перемещение мыши с нажатой правой кнопкой может
 			       использоваться для перемещения графика.
 
 			       Дополнительные параметры, такие как число шагов (nsteps),
 			       начальное значение t (tinitial), центры и радиусы x и y
 			       могут быть установлены в меню Настройка.
 
 			       Вы можете распечатать на PostScript-принтере или сохранить 
 				   график в PostScript-файле нажатием на Сохранить. Для выбора 
 				   между печатью и сохранением воспользуйтесь Настройка печати в меню Настройка.
                           }


##### Plotconf.tcl
mcset ru "Menu Here" "Меню"
mcset ru "Dismiss" "Закрыть"
mcset ru "Zoom" "Маштабировать"
mcset ru "Save" "Сохранить"
mcset ru "Mark" "Выделить"
mcset ru "Help" "Справка"
mcset ru "Replot" "Перерисовать"
mcset ru "Config" "Настройка"
mcset ru "Clicking the left mouse button on the plot menu \
	  (top left corner), will bring up a menu.  Holding down \
	  right mouse button and dragging will translate the plot." \
         "Щелчек мышью в левом верхнем уголн окна открывает меню. \
	  Перемещение графика выполняется при нажатой правой кнопке."
mcset ru "Click to Zoom\nShift+Click Unzoom" "Щелчок для увеличения\nShift+щелчок для уменьшения"
mcset ru "Ftp Dialog Box" "Диалоговое окно FTP"
mcset ru "host to write file on" "хост для записи файла "
mcset ru "host to write to via" "хост для записи через "
mcset ru "Your User ID on host" "Ваш UID "
mcset ru "Your password on host" "Ваш пароль "
mcset ru "remote subdirectory for output" "удалённый каталог для вывода"
mcset ru "filename " "имя файла "
mcset ru "chapter " "раздел "
mcset ru "section" "секция "
mcset ru "Problem number" "Номер задачи "
mcset ru "Send it" "Отослать"
mcset ru "Cancel" "Прервать"
mcset ru "Failed to specify" "Не задано"
mcset ru "The variable %s appeared in %s but was not in allowed variables: %s or in parameters: %s" \
         "Переменная %s присутствует в %s, но не является допустимой переменной: %s или параметром: %s"
mcset ru "The form %s may involve variables other than %s or the parameters %s, or the latter may have invalid expressions: %s" \
         "Форма %s может содержать переменные, отличные от %s или параметров %s, или последние могут содержать ошибочные выражения: %s"
mcset ru "only wanted one function:" "ожидается только одна функция:"
mcset ru "different variables" "различные переменные"
mcset ru "Plot Setup" "Установки Графика"
mcset ru "Print Options" "Настройки Печати"


##### Plot2d.tcl
mcset ru "Schelter's 2d Plot Window" "Окно 2D графики Вильяма Шелтера"
mcset ru "Number of mesh grids" "Число делений сетки"
mcset ru \
			   {

			       William Schelter's plotter for two dimensional graphics.

			       to QUIT this HELP click here.

			       By clicking on Zoom, the mouse now allows you to zoom \
				   in on a region of the plot.  Each click near a point \
				   magnifies the plot, keeping the center at the point \
				   you clicked.  Depressing the SHIFT key while clicking \
				   zooms in the opposite direction.

			       To change the functions plotted, click on Config and \
				   enter new values in the entry windows, and then click on \
				   Replot in the main menu bar.

			       Holding the right mouse button down allows you to drag
			       (translate) the plot sideways or up and down.

			       Additional parameters such as the number of steps (nsteps), \
				   and the x and y centers and radii, may be set under the \
				   Config menu.

			       You may print to a postscript printer, or save the plot \
				   as a postscript file, by clicking on save.   To change \
				   between printing and saving see the Print Options under Config.
			
			   } \
			   {

 			       Построитель двумерной графики Вильяма Шелтера.

 			       Для выхода из этой подсказки нажмите здесь.

 			       При нажатии на Масштабировать, можно масштабировать мышью \
 				   нарисованный участок. Каждый щелчок мышью увеличивает\
 				   изображение, сохраняя центр в точке нажатия. Удержание \
 				   клавиши SHIFT в момент щелчка уменьшает изображение.
 
 			       Для изменения рисуемой функции нажмите на Настройка и \
 				   введите новые значения в поля ввода, а затем нажмите на \
 				   Перерисовать в главном меню.
 
 			       Удерживание правой кнопки мыши позволит Вам перенести \
 			       (транслировать) график в стороны, вверх или вниз.
 
 			       Дополнительные параметры, такие как количество шагов (nsteps), \
 				   координаты центра и радиусы можно установить \
 				   в меню Настройка.
 
 			       Вы можете распечатать на PostScript-принтере или сохранить \
 				   график в PostScript-файле нажатием на Сохранить. Для выбора \
 				   между печатью и сохранением воспользуйтесь Настройка печати в Настройка.
			  }

mcset ru "Extra Plotting Information" "Дополнительная информация о графике"
mcset ru "Plot Function f(x)" "Рисуемая функция f(x)"
mcset ru "plot color" "Цвет графика"


##### Plot3d.tcl
mcset ru \
			  {

			       William Schelter's plotter for three dimensional graphics.

			       To QUIT this HELP click here.

			       By clicking on Zoom, the mouse now allows you \
				   to zoom in on a region of the plot.  Each click \
				   near a point magnifies the plot, keeping the \
				   center at the point you clicked.  Depressing \
				   the SHIFT key while clicking zooms in the \
				   opposite direction.

			       Clicking on Rotate, makes the left mouse button  \
				   cause rotation of the image.   The current position \
				   can be determined by azimuth and elevation angles \
				   which are given under the Config menu.   They may also \
				   be specified on the command line.

			       To change the equations enter in the entry \
				   windows, and click on replot.

			       You may print to a postscript printer, or save the plot \
				   as a postscript file, by clicking on save.   To change \
				   between printing and saving see the Print Options under Config.

			       Clicking with the right mouse button and dragging may be used \
				   instead of the scroll bars to slide the plot \
				   around.

			   } \
			   {

 			       Построитель Вильяма Шелтера для трехмерной графики.

 			       Для выхода из этой подсказки нажмите здесь.

 			       При нажатии на Масштабировать, можно масштабировать мышью \
 				   нарисованный участок. Каждый щелчок мышью увеличивает \
 				   изображение, сохраняя центр в точке нажатия. Удержание \
 				   клавиши SHIFT в момент щелчка уменьшает изображение.

 			       При нажатии на Вращение, движение с нажатой левой кнопкой \
 				   мыши вращает изображение. Текущую позицию можно \
 				   задать углами азимута и склонения в меню Настройка. \
 				   Также они могут быть заданы в командной строке.

 			       Для изменения уравнений введите их в соответствующие строки \
 				   ввода и нажмите Перерисовать.

 			       Вы можете распечатать на PostScript-принтере или сохранить \
 				   график в PostScript-файле нажатием на Сохранить. Для выбора \
 				   между печатью и сохранением воспользуйтесь Настройка печати в Настройка.

 			       Перемещение мыши с нажатой правой кнопкой может использоваться \
 				   для перемещения графика (вместо использования полос прокрутки).

			   }
mcset ru "Schelter's 3d Plot Window" "Окно 3D графики Вильяма Шелтера"
mcset ru "Rotate" "Вращение"
mcset ru {Dragging the mouse with the left button depressed will cause the object to rotate.  The rotation keeps the z axis displayed in an upright position (ie parallel to the sides of the screen), but changes the viewpoint.   Moving right and left changes the azimuth (rotation about the z axis), and up and down changes the elevation (inclination of z axis).   The red,blue and green sides of the bounding box are parallel to the X, Y and Z axes, and are on the smaller side.} \
         {Передвижение мыши с нажатой левой кнопкой вызовет вращение объекта. Вращение сохраняет прямое направление оси z (т.е. параллельно сторонам экрана), но изменяет положение наблюдателя. Перемещение вправо и влево меняет азимут (вращение вокруг оси z), вверх и вниз меняет склонение (отклонение от оси z).   Красная, синяя и зеленая стороны ограничивающего параллелепипеда параллельны осям на X, Y и Z, и находятся на меньшей стороне.}
mcset ru "azimuth" "азимут"
mcset ru "elevation" "склонение"


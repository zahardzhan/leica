# Лейка

Многопоточная качалка для [датакода](http://data.cod.ru/) и
файлообменников местного [dsvload.net](http://dsvload.net/).

## На чём это работает?

Работает везде, однако требует Java Runtime Environment.

## Как установить?

Установки не требует. Всего-то нужно скачать свежую сборку, и можно пользоваться.

    # wget http://cloud.github.com/downloads/zahardzhan/leica/leica-standalone.jar

## Как пользоваться?

Вам нужен текстовый файл с адресами, которые вы хотите скачать, в разумных 
пределах разбавленный мусором:

    Местный FTP по протоколу HTTP: http://77.35.112.84/incoming/PersonalFolders/User/Film.avi
    DSV data.cod: http://dsv.data.cod.ru/666666
    AMUR data.cod: http://amur.data.cod.ru/4444
    KHV data.cod (нужна помощь в тестировании): http://khv.data.cod.ru/123
    Еще какой-нибудь data.cod: http://data.cod.ru/12345

Просто запустите:

    # java -jar leica-standalone.jar [файл с адресами чего качать] [директория куда всё это качать]

## Сборка из исходников.

Если вам вдруг захотелось самую горяченькую (и не факт, что работающую) лейку,
вы можете собрать её сами из исходников. Я использую Ubuntu, поэтому всё ниже написанное
относится к этой системе.

Лейка лежит на git-репозитории, поэтому нужно поставить систему контроля версий git.
Для сборки лейки нужно установить язык clojure (впрочем, можно ограничиться JDK) и
систему сборки maven:

    # sudo aptitude install git-core clojure maven2

После установки клонируете репозиторий лейки себе:

    # git clone git://github.com/zahardzhan/leica.git leica

Подготавливаете систему сборки clojure-проектов leiningen к сборке:

    # ./lein self-install
    # ./lein deps

И собираете лейку:

    # ./lein clean
    # ./lein uberjar

## Напоследок.

Лейка является свободным программным обеспечением и распространяется
в надежде на то, что она будет вам полезной, однако я НЕ ПРЕДОСТАВЛЯЮ 
НА НЕЕ НИКАКИХ ГАРАНТИЙ.

Автор (c) 2009 Роман Захаров <zahardzhan@gmail.com>

Под лицензией GPL.

Copyright (C) 2009 Roman Zaharov <zahardzhan@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program. If not, see http://www.gnu.org/licenses/

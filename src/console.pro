QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets webkitwidgets

TARGET = console
TEMPLATE = app

SOURCES += main.cpp\
    Erlcmd.cpp \
    ErlangConsole.cpp \
    ConsoleWidget.cpp \
    SwitcherWidget.cpp \
    KeyHandler.cpp

HEADERS  += \
    Erlcmd.h \
    ErlangConsole.h \
    ConsoleWidget.h \
    SwitcherWidget.h \
    KeyHandler.h

FORMS    +=

LIBS += -lerl_interface -lei

target.path = /usr/bin
INSTALLS += target

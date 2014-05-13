QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets webkitwidgets

TARGET = console
TEMPLATE = app

SOURCES += main.cpp\
    Erlcmd.cpp \
    ConsoleWidget.cpp \
    SwitcherWidget.cpp \
    KeyHandler.cpp \
    DtachClient.cpp \
    ErlWebView.cpp

HEADERS  += \
    Erlcmd.h \
    ConsoleWidget.h \
    SwitcherWidget.h \
    KeyHandler.h \
    DtachClient.h \
    ErlWebView.h

FORMS    +=

LIBS += -lerl_interface -lei

target.path = /usr/bin
INSTALLS += target

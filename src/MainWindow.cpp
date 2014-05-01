#include "MainWindow.h"

#include "ConsoleWidget.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    ConsoleWidget *c = new ConsoleWidget(this);

    setCentralWidget(c);

    this->setMinimumSize(640, 480);
}

MainWindow::~MainWindow()
{
}


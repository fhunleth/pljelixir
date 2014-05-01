#include "MainWindow.h"
#include <QApplication>

#include "ErlangConsole.h"
#include "ConsoleWidget.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    ConsoleWidget c;
    ErlangConsole erlangConsole(&c);

    c.setMinimumSize(640, 480);
    c.show();
    
    return a.exec();
}

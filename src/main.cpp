#include "MainWindow.h"
#include <QApplication>
#include <QWebView>

#include "ErlangConsole.h"
#include "ConsoleWidget.h"
#include "SwitcherWidget.h"
#include "SwitchHelper.h"

// Uncomment to test standalone (i.e. not being called from Erlang)
//#define TEST

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    QWebView *webView = new QWebView();
    webView->setUrl(QUrl("file:///home/fhunleth/experiments/webtest/web/index.html"));

    ConsoleWidget *c = new ConsoleWidget();
#ifdef TEST
    c->printPrompt();
#else
    ErlangConsole erlangConsole(c, webView);
#endif

    SwitcherWidget switcher;
    switcher.addWidget(webView);
    switcher.addWidget(c);
    switcher.setMinimumSize(800, 600);
    switcher.show();

    SwitchHelper helper(&switcher);
    a.installEventFilter(&helper);

    // On EGL,
    switcher.setFocus();

    return a.exec();
}

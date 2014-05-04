#include "MainWindow.h"
#include <QApplication>
#include <QWebView>

#include "ErlangConsole.h"
#include "ConsoleWidget.h"
#include "SwitcherWidget.h"
#include "SwitchHelper.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    QWebView *webView = new QWebView();
    webView->setUrl(QUrl("file:///home/fhunleth/experiments/webtest/web/index.html"));

    ConsoleWidget *c = new ConsoleWidget();
    ErlangConsole erlangConsole(c, webView);

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

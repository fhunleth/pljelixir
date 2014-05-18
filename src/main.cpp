#include <QApplication>
#include <QWebView>

#include "ErlWebView.h"
#include "ConsoleWidget.h"
#include "SwitcherWidget.h"
#include "KeyHandler.h"
#include "DtachClient.h"

// Uncomment to test standalone (i.e. not being called from Erlang)
//#define TEST

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    QWebView *webView = new QWebView();

    ConsoleWidget *c = new ConsoleWidget();
#ifdef TEST
    webView->setUrl(QUrl("file:///home/fhunleth/experiments/pljelixir/priv/html/index.html"));
#else
    ErlWebView erlWebView(webView);
#endif

    SwitcherWidget switcher;
    switcher.setFullscreenWidget(webView);
    switcher.setOverlayWidget(c);
    switcher.setMinimumSize(800, 600);
    switcher.show();

    KeyHandler helper(&switcher, webView);
    a.installEventFilter(&helper);

    // On EGL, it seems like we have to force focus
    switcher.forceFocus();

    return a.exec();
}

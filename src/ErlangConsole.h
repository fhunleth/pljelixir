#ifndef ERLANGCONSOLE_H
#define ERLANGCONSOLE_H

#include <QObject>
#include "Erlcmd.h"

class ConsoleWidget;
class QWebView;

class ErlangConsole : public QObject
{
    Q_OBJECT
public:
    explicit ErlangConsole(ConsoleWidget *console, QWebView *webView, QObject *parent = 0);
    
private slots:
    void handleInput(const QString &input);
    void handleMessage(ETERMPtr msg);

private:
    Erlcmd *cmdprocessor_;
    ConsoleWidget *console_;
    QWebView *webView_;
};

#endif // ERLANGCONSOLE_H

#ifndef ERLANGCONSOLE_H
#define ERLANGCONSOLE_H

#include <QObject>
#include "Erlcmd.h"

class ConsoleWidget;

class ErlangConsole : public QObject
{
    Q_OBJECT
public:
    explicit ErlangConsole(ConsoleWidget *console, QObject *parent = 0);
    
signals:
    
public slots:

private slots:
    void handleInput(const QString &input);
    void handleMessage(ETERMPtr msg);

private:
    Erlcmd *cmdprocessor_;
    ConsoleWidget *console_;
};

#endif // ERLANGCONSOLE_H

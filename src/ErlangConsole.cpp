#include "ErlangConsole.h"
#include "ConsoleWidget.h"

ErlangConsole::ErlangConsole(ConsoleWidget *console, QObject *parent) :
    QObject(parent),
    console_(console)
{
    cmdprocessor_ = new Erlcmd(this);
    connect(cmdprocessor_, SIGNAL(messageReceived(ETERMPtr)), SLOT(handleMessage(ETERMPtr)));
    connect(console_, SIGNAL(inputReceived(QString)), SLOT(handleInput(QString)));
}

void ErlangConsole::handleInput(const QString &input)
{
    QByteArray inputUtf8 = input.toUtf8();
    ETERM *estring = erl_mk_binary(inputUtf8.constData(), inputUtf8.length());
    ETERM *response = erl_format("{input,~w}", estring);
    cmdprocessor_->send(response);

    erl_free(response);
    erl_free(estring);
}

void ErlangConsole::handleMessage(ETERMPtr msg)
{
}

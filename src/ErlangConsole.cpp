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
    ETERM *emsg = msg.data();

    // Commands are of the form {Command, Arguments}:
    // { atom(), [term()] }

    ETERM *cmd = erl_element(1, emsg);
    ETERM *args = erl_element(2, emsg);
    if (cmd == NULL || args == NULL)
        qFatal("Expecting { cmd, args }");

    ETERM *resp = 0;
    if (strcmp(ERL_ATOM_PTR(cmd), "rc") == 0) {
        ETERM *erc = erl_hd(args);
        if (erc == NULL || !ERL_IS_BINARY(erc))
            qFatal("rc: didn't get string");

        console_->appendReturnCode(QString::fromUtf8((const char *) ERL_BIN_PTR(erc), ERL_BIN_SIZE(erc)));
    } else if (strcmp(ERL_ATOM_PTR(cmd), "prompt") == 0) {
        console_->prompt();
    } else {
        resp = erl_format("error");
    }

    if (resp) {
        cmdprocessor_->send(resp);
        erl_free_term(resp);
    }

    erl_free_term(cmd);
    erl_free_term(args);
}

#ifndef CONSOLEWIDGET_H
#define CONSOLEWIDGET_H

#include <QTextEdit>

class ConsoleWidget : public QTextEdit
{
    Q_OBJECT
public:
    explicit ConsoleWidget(QWidget *parent = 0);

    void appendReturnCode(const QString &str);
    void prompt();

signals:
    void inputReceived(QString);

protected:
    void keyPressEvent(QKeyEvent *e);

private:
    bool isOnEditLine() const;
    QString currentCommand() const;

private:
    QString prompt_;

    int promptBlockNumber_;
    int promptColumnNumber_;
    int promptPosition_;

    QTextCharFormat promptFormat_;
    QTextCharFormat rcFormat_;
};

#endif // CONSOLEWIDGET_H

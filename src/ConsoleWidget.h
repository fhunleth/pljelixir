#ifndef CONSOLEWIDGET_H
#define CONSOLEWIDGET_H

#include <QTextEdit>

class ConsoleWidget : public QTextEdit
{
    Q_OBJECT
public:
    explicit ConsoleWidget(QWidget *parent = 0);
    
signals:
    void inputReceived(QString);

protected:
    void keyPressEvent(QKeyEvent *e);

private:
    void printPrompt();
    bool isOnEditLine() const;
    QString currentCommand() const;

private:
    QString prompt_;

    int promptBlockNumber_;
    int promptColumnNumber_;
    int promptPosition_;
};

#endif // CONSOLEWIDGET_H

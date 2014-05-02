#include "ConsoleWidget.h"

#include <QKeyEvent>
#include <QTextBlock>

ConsoleWidget::ConsoleWidget(QWidget *parent) :
    QTextEdit(parent)
{
    setStyleSheet("background-color: black; color: white");

    rcFormat_.setBackground(Qt::black);
    rcFormat_.setForeground(QColor(0xaa, 0x55, 0x00));

    promptFormat_.setBackground(Qt::black);
    promptFormat_.setForeground(Qt::white);

    errorFormat_.setBackground(Qt::black);
    errorFormat_.setForeground(Qt::red);

    prompt_ = ">>>";
}

void ConsoleWidget::printResult(const QString &str)
{
    QTextCursor cursor = this->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(str, rcFormat_);
}

void ConsoleWidget::printError(const QString &str)
{
    QTextCursor cursor = this->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertText(str, errorFormat_);
}

void ConsoleWidget::printPrompt()
{
    QTextCursor cursor = this->textCursor();
    cursor.movePosition(QTextCursor::End);
    cursor.insertBlock();
    cursor.insertText(prompt_, promptFormat_);

    promptBlockNumber_ = textCursor().blockNumber();
    promptColumnNumber_ = textCursor().columnNumber();
    promptPosition_ = textCursor().position();
}

bool ConsoleWidget::isOnEditLine() const
{
    return textCursor().blockNumber() == promptBlockNumber_ &&
           textCursor().columnNumber() >= promptColumnNumber_;
}

QString ConsoleWidget::currentCommand() const
{
    QTextBlock block = document()->findBlockByNumber(promptBlockNumber_);
    return block.text().mid(promptColumnNumber_);
}

void ConsoleWidget::keyPressEvent(QKeyEvent *e)
{
    switch (e->key()) {
    case Qt::Key_F2:
        zoomIn();
        break;

    case Qt::Key_F3:
        zoomOut();
        break;

    case Qt::Key_Return:
    case Qt::Key_Enter:
        e->accept();

        if (isOnEditLine()) {
            textCursor().insertBlock();
            emit inputReceived(currentCommand());
        } else {
            // Move to the end.
            moveCursor(QTextCursor::End);
        }
        break;

    case Qt::Key_Backspace:
        e->accept();

        if (isOnEditLine()) {
            if (textCursor().columnNumber() > promptColumnNumber_)
                textCursor().deletePreviousChar();
        } else {
            // Move to the end.
            moveCursor(QTextCursor::End);
        }
        break;

    case Qt::Key_Delete:
        e->accept();

        if (isOnEditLine()) {
            textCursor().deleteChar();
        } else {
            // Move to the end.
            moveCursor(QTextCursor::End);
        }
        break;

    case Qt::Key_Left:
    case Qt::Key_Right:
    case Qt::Key_Up:
    case Qt::Key_Down:
    case Qt::Key_End:
    case Qt::Key_PageDown:
    case Qt::Key_PageUp:
        QTextEdit::keyPressEvent(e);
        break;

    case Qt::Key_Home:
        e->accept();
        if (isOnEditLine()) {
            moveCursor(QTextCursor::StartOfLine);
            for (int i = 0; i < promptColumnNumber_; i++)
                moveCursor(QTextCursor::NextCharacter);
        } else
            moveCursor(QTextCursor::StartOfLine);
        break;

    default:
        if (isOnEditLine())
            QTextEdit::keyPressEvent(e);
        else {
            e->accept();
            moveCursor(QTextCursor::End);
            insertPlainText(e->text());
        }
        break;
    }
}


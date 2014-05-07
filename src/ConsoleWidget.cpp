#include "ConsoleWidget.h"
#include "DtachClient.h"

#include <QKeyEvent>
#include <QTextBlock>
#include <ctype.h>

ConsoleWidget::ConsoleWidget(QWidget *parent) :
    QTextEdit(parent)
{
    setStyleSheet("background-color: black; color: white; font-family: DejaVu Sans Mono");

    ansiBg_ = 0;
    ansiFg_ = 7;
    inverse_ = false;

    client_ = new DtachClient(this);
    connect(client_, SIGNAL(dataReceived(QByteArray)), SLOT(dataReceived(QByteArray)));
    connect(client_, SIGNAL(error()), SLOT(error()));

    client_->attach("/tmp/myiex");
}

void ConsoleWidget::dataReceived(const QByteArray &data)
{
    for (int i = 0; i < data.count(); i++) {
        char c = data.at(i);
        if (escapeSequence_.isEmpty()) {
            // Not in an escape sequence
            if (c == '\e') {
                flushBuffer();
                escapeSequence_.append(c);
            } else
                buffer_.append(c);
        } else {
            // In an escape sequence
            escapeSequence_.append(c);
            if (processEscapeSequence(escapeSequence_))
                escapeSequence_.clear();
        }
    }
    escapeSequence_.clear();

    flushBuffer();
    ensureCursorVisible();
}

void ConsoleWidget::error()
{
    textCursor().insertText("Error from dtach!!");
}

void ConsoleWidget::flushBuffer()
{
    if (!buffer_.isEmpty()) {
        QTextCharFormat format;
        format.setBackground(ansiToColor(inverse_ ? ansiFg_ : ansiBg_));
        format.setForeground(ansiToColor(inverse_ ? ansiBg_ : ansiFg_));

        QString text = QString::fromUtf8(buffer_);
        QTextCursor cursor = this->textCursor();
        for (int i = 0; i < text.length(); i++) {
            QChar c = text.at(i);
            switch (c.unicode()) {
            case '\a':
                break;

            case '\b':
                cursor.deletePreviousChar();
                break;

            default:
//                qDebug("Insert: %c", c.unicode());
                cursor.insertText(QString(c), format);
                break;
            }
        }
        buffer_.clear();
    }
}

bool ConsoleWidget::processEscapeSequence(const QByteArray &seq)
{
    if (seq.length() < 3)
        return false;

    // Throw out unknown escape sequences.
    if (seq.at(0) != '\e' || seq.at(1) != '[')
        return true;

    // Must end with a letter
    char cmd = seq.at(seq.count() - 1);
    if (!isalpha(cmd))
        return false;

    const int maxNumbers = 5;
    int num[maxNumbers] = {0};
    int numCount = 0;

    for (int i = 2; i < seq.length() && numCount < maxNumbers; i++) {
        char c = seq.at(i);
        if (isdigit(c))
            num[numCount] = num[numCount] * 10 + (c - '0');
        else
            numCount++;
    }

    qDebug("%s: %c(%d): %d %d %d %d %d", seq.mid(1).constData(), cmd, numCount, num[0], num[1], num[2], num[3], num[4]);
    switch (cmd) {
    case 'm':
        if (numCount != 1)
            break;

        switch (num[0]) {
        case 0:
            ansiBg_ = 0;
            ansiFg_ = 7;
            inverse_ = false;
            break;
        case 1:
            if (ansiFg_ < 8)
                ansiFg_ += 8;
            break;
        case 2:
        case 22:
            if (ansiFg_ >= 8)
                ansiFg_ -= 8;
            break;
        case 7:
            inverse_ = true;
            break;

        case 27:
            inverse_ = false;
            break;

        case 30:
        case 31:
        case 32:
        case 33:
        case 34:
        case 35:
        case 36:
        case 37:
            ansiFg_ = num[0] - 30 + (ansiFg_ < 8 ?  0 : 8);
            break;

        case 40:
        case 41:
        case 42:
        case 43:
        case 44:
        case 45:
        case 46:
        case 47:
            ansiBg_ = num[0] - 30 + (ansiBg_ < 8 ?  0 : 8);
            break;
        }

        break;

    default:
        break;
    }
    return true;
}

QColor ConsoleWidget::ansiToColor(int code)
{
    switch (code) {
    case 0:  return QColor(0x00, 0x00, 0x00);
    case 1:  return QColor(0xaa, 0x00, 0x00);
    case 2:  return QColor(0x00, 0xaa, 0x00);
    case 3:  return QColor(0xaa, 0x55, 0x00);
    case 4:  return QColor(0x00, 0x00, 0xaa);
    case 5:  return QColor(0xaa, 0x00, 0xaa);
    case 6:  return QColor(0x00, 0xaa, 0xaa);
    case 7:  return QColor(0xaa, 0xaa, 0xaa);
    case 8:  return QColor(0x55, 0x55, 0x55);
    case 9:  return QColor(0xff, 0x55, 0x55);
    case 10: return QColor(0x55, 0xff, 0x55);
    case 11: return QColor(0xff, 0xff, 0x55);
    case 12: return QColor(0x55, 0x55, 0xff);
    case 13: return QColor(0xff, 0x55, 0xff);
    case 14: return QColor(0x55, 0xff, 0xff);
    default:
    case 15: return QColor(0xff, 0xff, 0xff);
    }
}

void ConsoleWidget::keyPressEvent(QKeyEvent *e)
{
    QByteArray ansi;
    e->accept();

    switch (e->key()) {
    case Qt::Key_F2:
        zoomIn();
        break;

    case Qt::Key_F3:
        zoomOut();
        break;

    case Qt::Key_Up:
        ansi = "\e[A";
        break;
    case Qt::Key_Down:
        ansi = "\e[B";
        break;
    case Qt::Key_Right:
        ansi = "\e[C";
        break;
    case Qt::Key_Left:
        ansi = "\e[D";
        break;
    default:
        ansi = e->text().toUtf8();
        break;
    }

    if (!ansi.isEmpty())
        client_->sendData(ansi);

#if 0
    case Qt::Key_Return:
    case Qt::Key_Enter:
        e->accept();

        if (isOnEditLine()) {
            command_.append(currentCommand());
            if (e->modifiers() & Qt::ControlModifier) {
                // Ctrl-Enter sends to command to Elixir
                emit inputReceived(command_);
                command_.clear();
                textCursor().insertBlock();
            } else
                printPrompt();
        } else {
            // Move to the end.
            moveCursor(QTextCursor::End);
        }
        break;

    case Qt::Key_Backspace:
        e->accept();printError

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

        // Navigation and selection keys to pass on to QTextEdit
    case Qt::Key_Insert:
    case Qt::Key_Alt:
    case Qt::Key_Shift:
    case Qt::Key_Control:
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

    case Qt::Key_C:
        if (e->modifiers() & Qt::ControlModifier) {
            reset();
            e->accept();
            break;
        }
        // else fall through
    default:
        if (isOnEditLine())
            QTextEdit::keyPressEvent(e);
        else {
            e->accept();
            moveCursor(QTextCursor::End);
            insertPlainText(e->text());
        }
        break;

#endif
}


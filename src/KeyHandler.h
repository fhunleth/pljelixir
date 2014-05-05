#ifndef SWITCHHELPER_H
#define SWITCHHELPER_H

#include <QObject>

class QWebView;
class SwitcherWidget;

class KeyHandler : public QObject
{
    Q_OBJECT
public:
    explicit KeyHandler(SwitcherWidget *switcher, QWebView *webView, QObject *parent = 0);

protected:
    bool eventFilter(QObject *, QEvent *);

private:
    SwitcherWidget *switcher_;
    QWebView *webView_;
};

#endif // SWITCHHELPER_H

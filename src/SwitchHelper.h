#ifndef SWITCHHELPER_H
#define SWITCHHELPER_H

#include <QObject>

class SwitcherWidget;

class SwitchHelper : public QObject
{
    Q_OBJECT
public:
    explicit SwitchHelper(SwitcherWidget *switcher, QObject *parent = 0);

protected:
    bool eventFilter(QObject *, QEvent *);

private:
    SwitcherWidget *switcher_;
};

#endif // SWITCHHELPER_H

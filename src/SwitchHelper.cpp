#include "SwitchHelper.h"
#include "SwitcherWidget.h"
#include <QKeyEvent>

SwitchHelper::SwitchHelper(SwitcherWidget *switcher, QObject *parent) :
    QObject(parent),
    switcher_(switcher)
{
}

bool SwitchHelper::eventFilter(QObject *, QEvent *e)
{
    if (e->type() == QEvent::KeyPress || e->type() == QEvent::KeyRelease) {
        QKeyEvent *keyEvent = static_cast<QKeyEvent*>(e);
        if (keyEvent->key() == Qt::Key_F1) {
            // Toggle on key press; just consume key releases
            if (e->type() == QEvent::KeyPress)
                switcher_->toggle();

            return true;
        }
    }
    return false;
}

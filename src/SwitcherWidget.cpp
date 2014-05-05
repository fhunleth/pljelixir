#include "SwitcherWidget.h"
#include <QResizeEvent>
#include <QKeyEvent>

SwitcherWidget::SwitcherWidget(QWidget *parent) :
    QWidget(parent),
    index_(-1)
{
}

void SwitcherWidget::addWidget(QWidget *child)
{
    // Hide the currently visible widget, since the new
    // one will be the active one now.
    if (index_ >= 0)
        widgets_.at(index_)->setVisible(false);

    child->setParent(this);
    child->setGeometry(0, 0, size().width(), size().height());
    widgets_.append(child);
    child->setVisible(true);
    child->setFocus();

    // The most recently added widget is the new top one now.
    index_++;
}

void SwitcherWidget::toggle()
{
    if (index_ < 0)
        return;

    // Hide the old one
    widgets_.at(index_)->setVisible(false);

    index_++;
    if (index_ >= widgets_.count())
        index_ = 0;

    // Show the new one
    widgets_.at(index_)->setVisible(true);

    forceFocus();
}

void SwitcherWidget::forceFocus()
{
    QWidget *top = widgets_.at(index_);
    top->raise();
    top->setFocus();

}

void SwitcherWidget::resizeEvent(QResizeEvent *e)
{
    int w = e->size().width();
    int h = e->size().height();

    foreach (QWidget *child, widgets_)
        child->setGeometry(0, 0, w, h);
}


#include "SwitcherWidget.h"
#include <QResizeEvent>
#include <QKeyEvent>

SwitcherWidget::SwitcherWidget(QWidget *parent) :
    QWidget(parent),
    index_(0)
{
}

void SwitcherWidget::addWidget(QWidget *child)
{
    child->setParent(this);
    child->setGeometry(0, 0, size().width(), size().height());
    widgets_.append(child);
    child->setFocus();

    // The most recently added widget is the new top one now.
    index_++;
}

void SwitcherWidget::toggle()
{
    index_++;
    if (index_ >= widgets_.count())
        index_ = 0;

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


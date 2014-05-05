#ifndef SWITCHERWIDGET_H
#define SWITCHERWIDGET_H

#include <QWidget>

class SwitcherWidget : public QWidget
{
    Q_OBJECT
public:
    explicit SwitcherWidget(QWidget *parent = 0);

    void addWidget(QWidget *child);
    void toggle();
    void forceFocus();

protected:
    void resizeEvent(QResizeEvent *);

private:
    QList<QWidget*> widgets_;
    int index_;
};

#endif // SWITCHERWIDGET_H

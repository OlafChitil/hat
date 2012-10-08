typedef struct stack_s
{
    void *element;
    struct stack_s *next;
} stack;

stack* stack_newEmptyStack();
stack* stack_newByCopyingStack(stack* theStack);
void stack_delete(stack* theStack);
void* stack_pop(stack* theStack);
void stack_push(stack* theStack, void *element);
int stack_size(stack* theStack);

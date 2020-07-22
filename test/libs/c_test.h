#ifndef C_TEST
#define C_TEST

struct CArrayDouble {
    int length;
    double *array;
};

struct CArrayInt {
    int length;
    int *array;
};

struct CArrayFloat {
    int length;
    float *array;
};

struct CListDouble {
    double value;
    struct CListDouble *next;
};

struct CListInt {
    int value;
    struct CListInt *next;
};

struct CListFloat {
    float value;
    struct CListFloat *next;
};

struct CArrayDouble *arrayDouble(void);
struct CArrayInt    *arrayInt(void);
struct CArrayFloat  *arrayFloat(void);

struct CListDouble *listDouble(void);
struct CListInt    *listInt(void);
struct CListFloat  *listFloat(void);

#endif // C_TEST

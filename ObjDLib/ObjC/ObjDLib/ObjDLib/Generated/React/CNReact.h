#import "objd.h"
#import "CNObserver.h"
@class CNDispatchQueue;
@class CNAtomicObject;
@class CNChain;

@class CNReact;
@class CNImReact;
@class CNMReact;
@class CNVal;
@class CNVar;
@class CNSimpleVar;
@class CNFeedbackVar;
@class CNLimitedVar;
@class CNSlot;
@class CNReactExpression;
@class CNMappedReact;
@class CNMappedReact2;
@class CNMappedReact3;
@class CNFlatMappedReact;
@class CNAsyncMappedReact;
@class CNAsyncMappedReact2;
@class CNAsyncMappedReact3;
@class CNReactFlag;



@interface CNReact : CNObservable_impl
+ (instancetype)react;
- (instancetype)init;
- (CNClassType*)type;
+ (CNReact*)applyValue:(id)value;
+ (CNReact*)applyA:(CNReact*)a f:(id(^)(id))f;
+ (CNReact*)applyA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
+ (CNReact*)applyA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
+ (CNReact*)asyncQueue:(CNDispatchQueue*)queue a:(CNReact*)a f:(id(^)(id))f;
+ (CNReact*)asyncA:(CNReact*)a f:(id(^)(id))f;
+ (CNReact*)asyncQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
+ (CNReact*)asyncA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
+ (CNReact*)asyncQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
+ (CNReact*)asyncA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
- (void)attachObserver:(CNObserver*)observer;
- (void)detachObserver:(CNObserver*)observer;
- (id)value;
- (CNReact*)mapF:(id(^)(id))f;
- (CNReact*)flatMapF:(CNReact*(^)(id))f;
- (CNReact*)asyncMapQueue:(CNDispatchQueue*)queue f:(id(^)(id))f;
- (CNReact*)asyncMapF:(id(^)(id))f;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNImReact : CNReact
+ (instancetype)imReact;
- (instancetype)init;
- (CNClassType*)type;
- (void)attachObserver:(CNObserver*)observer;
- (void)detachObserver:(CNObserver*)observer;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNMReact : CNReact<CNObservableBase> {
@public
    CNAtomicObject* __value;
    CNAtomicObject* __observers;
}
@property (nonatomic, readonly) CNAtomicObject* _value;

+ (instancetype)reactWithInitial:(id)initial;
- (instancetype)initWithInitial:(id)initial;
- (CNClassType*)type;
- (id)value;
- (void)_setValue:(id)value;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNVal : CNImReact {
@public
    id _value;
}
@property (nonatomic, readonly) id value;

+ (instancetype)valWithValue:(id)value;
- (instancetype)initWithValue:(id)value;
- (CNClassType*)type;
- (NSString*)description;
- (BOOL)isEqual:(id)to;
- (NSUInteger)hash;
+ (CNClassType*)type;
@end


@interface CNVar : CNMReact
+ (instancetype)varWithInitial:(id)initial;
- (instancetype)initWithInitial:(id)initial;
- (CNClassType*)type;
+ (CNVar*)applyInitial:(id)initial;
+ (CNVar*)limitedInitial:(id)initial limits:(id(^)(id))limits;
+ (CNVar*)feedbackInitial:(id)initial feedback:(void(^)(id))feedback;
- (void)setValue:(id)value;
- (void)updateF:(id(^)(id))f;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNSimpleVar : CNVar
+ (instancetype)simpleVarWithInitial:(id)initial;
- (instancetype)initWithInitial:(id)initial;
- (CNClassType*)type;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNFeedbackVar : CNVar {
@public
    void(^_feedback)(id);
}
@property (nonatomic, readonly) void(^feedback)(id);

+ (instancetype)feedbackVarWithInitial:(id)initial feedback:(void(^)(id))feedback;
- (instancetype)initWithInitial:(id)initial feedback:(void(^)(id))feedback;
- (CNClassType*)type;
- (void)setValue:(id)value;
- (void)feedValue:(id)value;
- (void)updateF:(id(^)(id))f;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNLimitedVar : CNVar {
@public
    id(^_limits)(id);
}
@property (nonatomic, readonly) id(^limits)(id);

+ (instancetype)limitedVarWithInitial:(id)initial limits:(id(^)(id))limits;
- (instancetype)initWithInitial:(id)initial limits:(id(^)(id))limits;
- (CNClassType*)type;
- (void)setValue:(id)value;
- (void)updateF:(id(^)(id))f;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNSlot : CNMReact {
@public
    CNReact* __base;
    CNObserver* __observer;
}
+ (instancetype)slotWithInitial:(id)initial;
- (instancetype)initWithInitial:(id)initial;
- (CNClassType*)type;
- (void)connectTo:(CNReact*)to;
- (void)setValue:(id)value;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNReactExpression : CNMReact
+ (instancetype)reactExpressionWithInitial:(id)initial;
- (instancetype)initWithInitial:(id)initial;
- (CNClassType*)type;
- (void)recalc;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNMappedReact : CNReactExpression {
@public
    CNReact* _a;
    id(^_f)(id);
    CNObserver* _obsA;
}
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)mappedReactWithA:(CNReact*)a f:(id(^)(id))f;
- (instancetype)initWithA:(CNReact*)a f:(id(^)(id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNMappedReact2 : CNReactExpression {
@public
    CNReact* _a;
    CNReact* _b;
    id(^_f)(id, id);
    CNObserver* _obsA;
    CNObserver* _obsB;
}
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) CNReact* b;
@property (nonatomic, readonly) id(^f)(id, id);

+ (instancetype)mappedReact2WithA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
- (instancetype)initWithA:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNMappedReact3 : CNReactExpression {
@public
    CNReact* _a;
    CNReact* _b;
    CNReact* _c;
    id(^_f)(id, id, id);
    CNObserver* _obsA;
    CNObserver* _obsB;
    CNObserver* _obsC;
}
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) CNReact* b;
@property (nonatomic, readonly) CNReact* c;
@property (nonatomic, readonly) id(^f)(id, id, id);

+ (instancetype)mappedReact3WithA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
- (instancetype)initWithA:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNFlatMappedReact : CNReactExpression {
@public
    CNReact* _a;
    CNReact*(^_f)(id);
    CNObserver* _obsA;
}
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) CNReact*(^f)(id);

+ (instancetype)flatMappedReactWithA:(CNReact*)a f:(CNReact*(^)(id))f;
- (instancetype)initWithA:(CNReact*)a f:(CNReact*(^)(id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNAsyncMappedReact : CNReactExpression {
@public
    CNDispatchQueue* _queue;
    CNReact* _a;
    id(^_f)(id);
    CNObserver* _obsA;
}
@property (nonatomic, readonly) CNDispatchQueue* queue;
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)asyncMappedReactWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a f:(id(^)(id))f;
- (instancetype)initWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a f:(id(^)(id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNAsyncMappedReact2 : CNReactExpression {
@public
    CNDispatchQueue* _queue;
    CNReact* _a;
    CNReact* _b;
    id(^_f)(id, id);
    CNObserver* _obsA;
    CNObserver* _obsB;
}
@property (nonatomic, readonly) CNDispatchQueue* queue;
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) CNReact* b;
@property (nonatomic, readonly) id(^f)(id, id);

+ (instancetype)asyncMappedReact2WithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
- (instancetype)initWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b f:(id(^)(id, id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNAsyncMappedReact3 : CNReactExpression {
@public
    CNDispatchQueue* _queue;
    CNReact* _a;
    CNReact* _b;
    CNReact* _c;
    id(^_f)(id, id, id);
    CNObserver* _obsA;
    CNObserver* _obsB;
    CNObserver* _obsC;
}
@property (nonatomic, readonly) CNDispatchQueue* queue;
@property (nonatomic, readonly) CNReact* a;
@property (nonatomic, readonly) CNReact* b;
@property (nonatomic, readonly) CNReact* c;
@property (nonatomic, readonly) id(^f)(id, id, id);

+ (instancetype)asyncMappedReact3WithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
- (instancetype)initWithQueue:(CNDispatchQueue*)queue a:(CNReact*)a b:(CNReact*)b c:(CNReact*)c f:(id(^)(id, id, id))f;
- (CNClassType*)type;
- (id)calc;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNReactFlag : CNMReact {
@public
    NSArray* _reacts;
    NSArray* _observers;
}
@property (nonatomic, readonly) NSArray* reacts;

+ (instancetype)reactFlagWithInitial:(BOOL)initial reacts:(NSArray*)reacts;
- (instancetype)initWithInitial:(BOOL)initial reacts:(NSArray*)reacts;
- (CNClassType*)type;
- (void)set;
- (void)setValue:(BOOL)value;
- (void)clear;
- (void)processF:(void(^)())f;
+ (CNReactFlag*)applyInitial:(BOOL)initial;
+ (CNReactFlag*)applyReacts:(NSArray*)reacts;
+ (CNReactFlag*)apply;
- (NSString*)description;
+ (CNClassType*)type;
@end



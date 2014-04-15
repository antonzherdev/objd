#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNTuple;
@class CNTuple3;
@class CNTuple4;
@class CNTuple5;

@interface CNTuple : NSObject<ODComparable> {
@protected
    id _a;
    id _b;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;

+ (instancetype)tupleWithA:(id)a b:(id)b;
- (instancetype)initWithA:(id)a b:(id)b;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple*)to;
- (NSString*)description;
+ (CNTuple*)unapplyTuple:(CNTuple*)tuple;
+ (ODClassType*)type;
@end


@interface CNTuple3 : NSObject<ODComparable> {
@protected
    id _a;
    id _b;
    id _c;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;
@property (nonatomic, readonly) id c;

+ (instancetype)tuple3WithA:(id)a b:(id)b c:(id)c;
- (instancetype)initWithA:(id)a b:(id)b c:(id)c;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple3*)to;
- (NSString*)description;
+ (CNTuple3*)unapplyTuple:(CNTuple3*)tuple;
+ (ODClassType*)type;
@end


@interface CNTuple4 : NSObject<ODComparable> {
@protected
    id _a;
    id _b;
    id _c;
    id _d;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;
@property (nonatomic, readonly) id c;
@property (nonatomic, readonly) id d;

+ (instancetype)tuple4WithA:(id)a b:(id)b c:(id)c d:(id)d;
- (instancetype)initWithA:(id)a b:(id)b c:(id)c d:(id)d;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple4*)to;
- (NSString*)description;
+ (CNTuple4*)unapplyTuple:(CNTuple4*)tuple;
+ (ODClassType*)type;
@end


@interface CNTuple5 : NSObject<ODComparable> {
@protected
    id _a;
    id _b;
    id _c;
    id _d;
    id _e;
}
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;
@property (nonatomic, readonly) id c;
@property (nonatomic, readonly) id d;
@property (nonatomic, readonly) id e;

+ (instancetype)tuple5WithA:(id)a b:(id)b c:(id)c d:(id)d e:(id)e;
- (instancetype)initWithA:(id)a b:(id)b c:(id)c d:(id)d e:(id)e;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple5*)to;
- (NSString*)description;
+ (CNTuple5*)unapplyTuple:(CNTuple5*)tuple;
+ (ODClassType*)type;
@end



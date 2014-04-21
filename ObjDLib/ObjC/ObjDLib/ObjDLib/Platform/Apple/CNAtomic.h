#import "objd.h"

@class CNAtomicBool;

@interface CNAtomicBool : NSObject
+ (id)atomicBool;
- (id)init;
- (ODClassType*)type;
- (BOOL)boolValue;
- (void)setNewValue:(BOOL)newValue;
- (BOOL)getAndSetNewValue:(BOOL)newValue;
- (BOOL)compareAndSetOldValue:(BOOL)oldValue newValue:(BOOL)newValue;
+ (ODClassType*)type;
@end


@interface CNAtomicInt : NSObject
+ (id)atomicInt;
- (id)init;
- (ODClassType*)type;
- (int)intValue;
- (void)setNewValue:(int)newValue;
- (int)incrementAndGet;
- (int)decrementAndGet;
- (int)addAndGetValue:(int)value;
- (BOOL)compareAndSetOldValue:(int)oldValue newValue:(int)newValue;
+ (ODClassType*)type;
@end

@interface CNAtomicObject : NSObject
- (instancetype)init;
- (instancetype)initWithValue:(id)value;
+ (instancetype)atomicObjectWithValue:(id)value;
- (ODClassType*)type;
- (id)value;
- (void)setNewValue:(id)newValue;
- (BOOL)compareAndSetOldValue:(id)oldValue newValue:(id)newValue;
+ (ODClassType*)type;
@end

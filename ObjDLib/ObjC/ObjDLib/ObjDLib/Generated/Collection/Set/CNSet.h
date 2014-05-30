#import "objdcore.h"
#import "CNCollection.h"
#import "CNObject.h"
@class CNClassType;
@class CNString;
@class CNDispatchQueue;
@class CNChain;
@class CNMHashSet;
@class CNImHashSet;

@class CNSet_impl;
@class CNImSet_impl;
@class CNMSet_impl;
@class CNHashSetBuilder;
@protocol CNSet;
@protocol CNImSet;
@protocol CNMSet;

@protocol CNSet<CNIterable>
- (NSString*)description;
@end


@interface CNSet_impl : CNIterable_impl<CNSet>
+ (instancetype)set_impl;
- (instancetype)init;
@end


@protocol CNImSet<CNSet, CNImIterable>
- (id<CNMSet>)mCopy;
- (NSString*)description;
@end


@interface CNImSet_impl : CNSet_impl<CNImSet>
+ (instancetype)imSet_impl;
- (instancetype)init;
- (id<CNMSet>)mCopy;
@end


@protocol CNMSet<CNSet, CNMIterable>
- (id<CNImSet>)im;
- (id<CNImSet>)imCopy;
- (NSString*)description;
@end


@interface CNMSet_impl : CNSet_impl<CNMSet>
+ (instancetype)set_impl;
- (instancetype)init;
- (id<CNImSet>)im;
- (id<CNImSet>)imCopy;
@end


@interface CNHashSetBuilder : CNBuilder_impl {
@protected
    CNMHashSet* _set;
}
@property (nonatomic, readonly) CNMHashSet* set;

+ (instancetype)hashSetBuilderWithCapacity:(NSUInteger)capacity;
- (instancetype)initWithCapacity:(NSUInteger)capacity;
- (CNClassType*)type;
- (void)appendItem:(id)item;
- (CNImHashSet*)build;
+ (CNHashSetBuilder*)apply;
- (NSString*)description;
+ (CNClassType*)type;
@end



#import "objd.h"

@class Foo;

@interface Foo : NSObject
+ (instancetype)fooWithBar:(NSInteger)bar;
- (instancetype)initWithBar:(NSInteger)bar;
- (CNClassType*)type;
- (NSString*)description;
+ (CNClassType*)type;
@end



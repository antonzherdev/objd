#import "objd.h"

@class ODEnum;

@interface ODEnum : NSObject
@property (nonatomic, readonly) NSUInteger ordinal;
@property (nonatomic, readonly) NSString* name;

+ (id)enumWithOrdinal:(NSUInteger)ordinal name:(NSString*)name;
- (id)initWithOrdinal:(NSUInteger)ordinal name:(NSString*)name;
- (NSString*)description;
- (BOOL)isEqual:(id)other;
- (NSUInteger)hash;

- (NSInteger)compareTo:(ODEnum *)to;
@end



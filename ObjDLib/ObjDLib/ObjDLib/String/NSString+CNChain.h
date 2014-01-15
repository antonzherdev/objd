#import <Foundation/Foundation.h>
#import "CNSeq.h"

@protocol CNIterable;

@interface NSString (CNChain) <CNSeq, ODComparable>
- (id)tupleBy:(NSString *)by;

- (id <CNIterable>)splitBy:(NSString *)by;
- (NSUInteger) toUInt;
- (NSInteger) toInt;
- (CGFloat) toFloat;
- (NSString *)encodeForURL;

- (NSString *)replaceOccurrences:(NSString *)from withString:(NSString *)to;

- (id)substrBegin:(NSUInteger)begin end:(NSUInteger)end;
@end
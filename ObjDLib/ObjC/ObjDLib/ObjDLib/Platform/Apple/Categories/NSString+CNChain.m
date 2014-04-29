#import "NSString+CNChain.h"
#import "NSArray+CNChain.h"
#import "CNChain.h"
#import "CNSet.h"
#import "CNDispatchQueue.h"

@interface CNSplitByIterator : NSObject<CNIterator>
- (id)initWithString:(NSString *)string by:(NSString *)by;

+ (id)iteratorWithString:(NSString *)string by:(NSString *)by;

@end

@implementation CNSplitByIterator {
    NSString* _string;
    NSString* _by;
    NSRange _range;
    NSUInteger _length;
}
- (id)initWithString:(NSString *)string by:(NSString *)by {
    self = [super init];
    if (self) {
        _string = string;
        _length = _string.length;
        _range.length = _length;
        _range.location = 0;
        _by = by;
    }

    return self;
}

- (BOOL)hasNext {
    return _string != nil && _range.length != 0;
}

- (id)next {
    NSRange foundedRange = [_string rangeOfString:_by options:0 range:_range];
    if(foundedRange.length <= 0) {
        NSString *r = [_string substringWithRange:_range];
        _string = nil;
        return r;
    } else {
        NSRange rng = {_range.location, foundedRange.location - _range.location};
        NSString *r = [_string substringWithRange:rng];
        _range.location = foundedRange.location + foundedRange.length;
        _range.length = _length - _range.location;
        return r;
    }
}

+ (id)iteratorWithString:(NSString *)string by:(NSString *)by {
    return [[self alloc] initWithString:string by:by];
}
@end

@implementation NSString (CNChain)
- (id)tupleBy:(NSString *)by {
    NSRange range = [self rangeOfString:by];
    if(range.length <= 0) return nil;
    return [CNTuple tupleWithA:[self substringToIndex:range.location] b:[self substringFromIndex:range.location + range.length]];
}

- (id <CNIterable>)splitBy:(NSString *)by {
    return [CNIterableF iterableFWithIteratorF:^id <CNIterator> {
        return [CNSplitByIterator iteratorWithString:self by:by];
    }];
}

- (CGFloat)toFloat {
    return [self floatValue];
}

- (NSInteger)toInt {
    return [self integerValue];
}

- (NSUInteger)toUInt {
    return (NSUInteger) [self integerValue];
}


- (id)applyIndex:(NSUInteger)index1 {
    if(index1 >= [self length]) @throw @"Incorrect index";
    return nums([self characterAtIndex:index1]);
}

- (NSUInteger)count {
    return self.length;
}

- (id <CNIterator>)iterator {
    return [CNIndexFunSeqIterator indexFunSeqIteratorWithCount:self.length f:^id(NSUInteger i) {
        return nums([self characterAtIndex:i]);
    }];
}

- (id)optIndex:(NSUInteger)index1 {
    if(index1 >= [self length]) return nil;
    return nums([self characterAtIndex:index1]);
}

- (id)randomItem {
    if([self isEmpty]) return nil;
    else return [self applyIndex:cnuIntRndMax([self count] - 1)];
}

- (id<CNSet>)toSet {
    return [self convertWithBuilder:[CNHashSetBuilder hashSetBuilder]];
}

- (id<CNImSeq>)addItem:(id)item {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return ((NSArray*)([builder build]));
}

- (id <CNImSeq>)addSeq:(id <CNSeq>)seq {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilder];
    [builder appendAllItems:self];
    [builder appendAllItems:seq];
    return ((NSArray*)([builder build]));
}

- (id <CNMSeq>)mCopy {
    return (id <CNMSeq>) [self mutableCopy];
}


- (id<CNImSeq>)subItem:(id)item {
    return [[[self chain] filter:^BOOL(id _) {
        return !([_ isEqual:item]);
    }] toArray];
}

- (BOOL)isEqualSeq:(id<CNSeq>)seq{
    if([self count] != [seq count]) return NO;
    id<CNIterator> ia = [self iterator];
    id<CNIterator> ib = [seq iterator];
    while([ia hasNext] && [ib hasNext]) {
        if(!([[ia next] isEqual:[ib next]])) return NO;
    }
    return YES;
}

- (BOOL)isEmpty {
    return [self length] == 0;
}

- (id)head {
    if([self length] == 0) return nil;
    return nums([self characterAtIndex:0]);
}

- (id <CNImSeq>)tail {
    return [self substringFromIndex:1];
}

- (id)last {
    return nums([self characterAtIndex:[self length]]);
}

- (CNChain*)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        each([i next]);
    }
}

- (void)parForEach:(void(^)(id))each {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        [[CNDispatchQueue aDefault] asyncF:^{
            each([i next]);
        }];
    }
}

- (BOOL)goOn:(BOOL(^)(id))on {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if(!(on([i next]))) return NO;
    }
    return YES;
}

- (BOOL)containsItem:(id)item {
    id<CNIterator> i = [self iterator];
    while([i hasNext]) {
        if([[i next] isEqual:i]) return YES;
    }
    return NO;
}

- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = nil;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = x;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(x)) {
            ret = YES;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    __block BOOL ret = YES;
    [self goOn:^BOOL(id x) {
        if(!confirm(numb(ret))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
}

- (NSString *)encodeForURL {
    NSMutableString * output = [NSMutableString string];
    const char * source = [self UTF8String];
    int sourceLen = (int)strlen(source);
    for (int i = 0; i < sourceLen; ++i) {
        const unsigned char thisChar = (const unsigned char)source[i];
        /*if (false && thisChar == ' '){
            [output appendString:@"+"];
        } else*/ if (thisChar == '.' || thisChar == '-' || thisChar == '_' || thisChar == '~' ||
                (thisChar >= 'a' && thisChar <= 'z') ||
                (thisChar >= 'A' && thisChar <= 'Z') ||
                (thisChar >= '0' && thisChar <= '9')) {
            [output appendFormat:@"%c", thisChar];
        } else {
            [output appendFormat:@"%%%02X", thisChar];
        }
    }
    return output;
}

- (NSString *)replaceOccurrences:(NSString *)from withString:(NSString *)to {
    return [self stringByReplacingOccurrencesOfString:from withString:to];
}

- (NSInteger)compareTo:(id)to {
    return [self compare:to];
}

- (id)substrBegin:(NSUInteger)begin end:(NSUInteger)end {
    return [self substringWithRange:NSMakeRange(begin, end - begin)];
}
@end


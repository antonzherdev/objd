#import "CNChain.h"
#import "CNSourceLink.h"
#import "CNFilterLink.h"
#import "CNMapLink.h"
#import "CNMulLink.h"
#import "CNTreeMap.h"
#import "CNCombinationsLink.h"
#import "CNGroupByLink.h"
#import "CNSortLink.h"
#import "CNSortBuilder.h"
#import "CNZipLink.h"
#import "CNTreeSet.h"
#import "CNFuture.h"
#import "CNFutureEnd.h"
#import "CNDispatchQueue.h"
#import "CNList.h"
#import "CNSeed.h"


@implementation CNChain {
    id<CNChainLink> _link;
    CNChain* _previous;
}

- (id)initWithLink:(id <CNChainLink>)link previous:(CNChain *)previous {
    self = [super init];
    if (self) {
        _link = link;
        _previous = previous;
    }

    return self;
}

+ (id)chainWithLink:(id <CNChainLink>)link previous:(CNChain *)previous {
    return [[self alloc] initWithLink:link previous:previous];
}

- (CNYield *)buildYield:(CNYield *)yield {
    if(_previous == nil ) return [_link buildYield:yield];
    return [_previous buildYield:[_link buildYield:yield]];
}


+ (CNChain*)chainWithCollection:(id)collection {
    return [CNChain chainWithLink:[CNSourceLink sourceLinkWithCollection:collection] previous:nil];
}

- (NSArray *)toArray {
    __block id ret;
    CNYield *yield = [CNYield alloc];
    __weak CNYield * wy = yield;
    yield = [yield initWithBegin:^CNGoR(NSUInteger size) {
        ret = [NSMutableArray arrayWithCapacity:size];
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [ret addObject:item];
        return CNGo_Continue;
    } end:nil all:^CNGoR(id collection) {
        if([collection isKindOfClass:[NSArray class]]) {
            ret = collection;
            return CNGo_Continue;
        }
        return [wy stdYieldAllItems:collection];
    }];
    [self apply:yield];
    return ret;
}

- (NSSet *)toSet {
    __block id ret;
    CNYield *yield = [CNYield alloc];
    __weak CNYield * wy = yield;
    yield = [yield initWithBegin:^CNGoR(NSUInteger size) {
        ret = [NSMutableSet setWithCapacity:size];
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [ret addObject:item];
        return CNGo_Continue;
    } end:nil all:^CNGoR(id collection) {
        if ([collection isKindOfClass:[NSSet class]]) {
            ret = collection;
            return CNGo_Continue;
        }
        return [wy stdYieldAllItems:collection];
    }];
    [self apply:yield];
    return ret;
}

- (id)foldStart:(id)start by:(cnF2)by {
    __block id ret = start;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:nil yield:^CNGoR(id item) {
        ret = by(ret, item);
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}

- (id )findWhere:(cnPredicate)predicate {
    __block id ret = nil;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:nil yield:^CNGoR(id item) {
        if(predicate(item)) {
            ret = item;
            return CNGo_Break;
        }
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}


- (CNChain *)filter:(cnPredicate)predicate {
    return [self link:[CNFilterLink filterLinkWithPredicate:predicate factor:0]];
}

- (CNChain *)filter:(cnPredicate)predicate selectivity:(double)selectivity {
    return [self link:[CNFilterLink filterLinkWithPredicate:predicate factor:selectivity]];
}

- (CNChain *)filterCast:(CNType *)type {
    return [self filter:^BOOL(id x) {
        return [x isKindOfClass:[type cls]];
    }];
}

- (CNChain *)map:(cnF)f {
    return [self link:[CNMapLink mapLinkWithF:f]];
}

- (CNChain *)flatMap:(cnF)f {
    return [self link:[CNFlatMapLink flatMapLinkWithF:f factor:2]];
}

- (CNChain *)flatMap:(cnF)f factor:(double)factor {
    return [self link:[CNFlatMapLink flatMapLinkWithF:f factor:factor]];
}


- (CNChain *)neighbours {
    return [self link:[CNNeighboursLink neighboursLinkWithRing:NO]];
}

- (CNChain *)neighboursRing {
    return [self link:[CNNeighboursLink neighboursLinkWithRing:YES]];
}


- (CNChain *)combinations {
    return [self link:[CNCombinationsLink combinationsLink]];
}

- (CNChain *)uncombinations {
    return [self link:[CNUncombinationsLink uncombinationsLink]];
}

- (CNChain *)groupBy:(cnF)by fold:(cnF2)fold withStart:(cnF0)start {
    return [self link:[CNImGroupByLink imGroupByLinkWithBy:by start:start fold:fold factor:0.5]];
}

- (CNChain *)groupBy:(cnF)by withBuilder:(cnF0)builder {
    return [self link:[CNMGroupByLink groupByLinkWithBy:by start:builder append:^void(id r, id x) {
        [r appendItem:x];
    } finish:^id(id x) {
        return [x build];
    } factor:0.5]];
}

- (CNChain *)groupBy:(cnF)by map:(cnF)f withBuilder:(cnF0)builder {
    return [self link:[CNMGroupByLink groupByLinkWithBy:by start:builder append:^void(id r, id x) {
        [r appendItem:f(x)];
    } finish:^id(id x) {
        return [x build];
    } factor:0.5]];
}


- (CNChain *)groupBy:(cnF)by map:(cnF)f {
    return [self groupBy:by map: f withBuilder:^id {
        return [CNArrayBuilder arrayBuilder];
    }];
}

- (CNChain *)groupBy:(cnF)by {
    return [self groupBy:by withBuilder:^id {
        return [CNArrayBuilder arrayBuilder];
    }];
}

- (CNChain *)zipA:(id <CNIterable>)a {
    return [self zipA:a by:^id(id x, id y) {
        return tuple(x, y);
    }];
}


- (CNChain *)zipA:(id <CNIterable>)a by:(id (^)(id, id))by {
    return [self link:[CNZipLink zipLinkWithA:a f:by]];
}

- (void)zipForA:(id <CNIterable>)a by:(void (^)(id, id))by {
    id <CNIterator> ai = [a iterator];
    [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        if(![ai hasNext]) return CNGo_Break;
        else {
            by(item, [ai next]);
            return CNGo_Continue;
        }
    } end:nil all:nil]];
}

- (CNChain *)zip3A:(id <CNIterable>)a b:(id <CNIterable>)b {
    return [self zip3A:a b:b by:^id(id x, id y, id z) {
        return tuple3(x, y, z);
    }];
}


- (CNChain *)zip3A:(id <CNIterable>)a b:(id <CNIterable>)b by:(cnF3)by {
    return [self link:[CNZip3Link zip3LinkWithA:a b:b f:by]];
}

- (CNChain *)append:(id)collection {
    return [self link:[CNAppendLink appendLinkWithCollection:collection]];
}

- (CNChain *)prepend:(id)collection {
    return [self link:[CNPrependLink prependLinkWithCollection:collection]];
}

- (CNChain *)exclude:(id)collection {
    id col = cnResolveCollection(collection);
    return [self filter:^BOOL(id x) {
        return ![col containsObject:x];
    }];
}

- (CNChain *)intersect:(id)collection {
    id col = cnResolveCollection(collection);
    return [self filter:^BOOL(id x) {
        return [col containsObject:x];
    }];
}


- (CNChain *)mul:(id)collection {
    return [self link:[CNMulLink mulLinkWithCollection:collection]];
}

- (CNChain *)reverse {
    return [self link:[CNReverseLink reverseLink]];
}

- (CNChain *)sort {
    return [self sort:^NSInteger(id x, id y) {
        return [x compareTo:y];
    }];
}

- (CNChain *)sort:(cnCompare)comparator {
    return [self link:[CNSortLink sortLinkWithComparator:comparator]];
}

- (CNChain *)sortDesc {
    return [self sort:^NSInteger(id x, id y) {
        return -[x compareTo:y];
    }];
}

- (void)forEach:(cnP)p {
    [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        p(item);
        return CNGo_Continue;
    } end:nil all:nil]];
}

- (void)parForEach:(void (^)(id))each {
    [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        [[CNDispatchQueue aDefault] asyncF:^{
            each(item);
        }];
        return CNGo_Continue;
    } end:nil all:nil]];
}


- (CNGoR)goOn:(CNGoR(^)(id))on {
    return [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        return on(item);
    } end:nil all:nil]] == 0;
}

- (CNChain *)chain {
    return self;
}

- (id)head {
    __block id ret = nil;
    [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        ret = item;
        return CNGo_Break;
    } end:nil all:nil]];
    return ret;
}

- (id)last {
    __block id ret = nil;
    [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        ret = item;
        return CNGo_Continue;
    } end:nil all:nil]];
    return ret;
}

- (id)convertWithBuilder:(id <CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
}

- (id)randomItem {
    if([_link isKindOfClass:[CNSourceLink class]]) {
        id collection = [(CNSourceLink *)_link collection];
        if([collection conformsToProtocol:@protocol(CNSeq)]) {
            NSUInteger nn = [collection count];
            if(nn == 0) return nil;
            NSUInteger ii = cnuIntRndMax(nn - 1);
            return [collection applyIndex:ii];
        }
    }
    NSArray *array = [self toArray];
    NSUInteger n = array.count;
    if(n == 0) return nil;

    NSUInteger i = cnuIntRndMax(n - 1);
    return [array objectAtIndex:i];
}

- (id)randomItemSeed:(CNSeed*)seed {
    if([_link isKindOfClass:[CNSourceLink class]]) {
        id collection = [(CNSourceLink *)_link collection];
        if([collection conformsToProtocol:@protocol(CNSeq)]) {
            NSUInteger nn = [collection count];
            if(nn == 0) return nil;
            NSUInteger ii = (NSUInteger) [seed nextIntMin:0 max:(int)nn - 1];
            return [collection applyIndex:ii];
        }
    }
    NSArray *array = [self toArray];
    NSUInteger n = array.count;
    if(n == 0) return nil;

    NSUInteger i = (NSUInteger) [seed nextIntMin:0 max:(int)n - 1];
    return [array objectAtIndex:i];
}

- (NSUInteger)count {
    __block NSUInteger ret = 0;
    [self apply:[CNYield yieldWithBegin:nil yield:^CNGoR(id item) {
        ret++;
        return CNGo_Continue;
    } end:nil all:nil]];
    return ret;
}


- (CNGoR)apply:(CNYield *)yield {
    CNYield *y = [self buildYield:yield];
    CNGoR result = [y beginYieldWithSize:0];
    return [y endYieldWithResult:result];
}

- (CNChain*)link:(id <CNChainLink>)link {
    return [CNChain chainWithLink:link previous:_link == nil ? nil : self];
}

- (id)min {
    __block id min = nil;
    [self forEach:^(id x) {
        if(min == nil || [x compareTo:min] < 0 ) min = x;
    }];
    return min;
}

- (id)max {
    __block id max = nil;
    [self forEach:^(id x) {
        if(max == nil || [x compareTo:max] > 0 ) max = x;
    }];
    return max;
}

- (NSDictionary *)toMap {
    return [self toMutableMap];
}

- (NSMutableDictionary *)toMutableMap {
    __block NSMutableDictionary* ret;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:^CNGoR(NSUInteger size) {
        ret = [NSMutableDictionary dictionaryWithCapacity:size];
        return CNGo_Continue;
    } yield:^CNGoR(CNTuple* item) {
        [ret setObject:item.b forKey:item.a];
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}

- (CNChain *)distinct {
    return [self link:[CNDistinctLink distinctLinkWithFactor:0.5]];
}

- (CNChain *)distinctWithSelectivity:(double)selectivity {
    return [self link:[CNDistinctLink distinctLinkWithFactor:selectivity]];
}

- (CNSortBuilder *)sortBy {
    return [CNSortBuilder sortBuilderWithChain:self];
}

- (NSString *)toStringWithDelimiter:(NSString *)delimiter {
    return [self toStringWithStart:@"" delimiter:delimiter end:@""];
}


- (NSString *)toStringWithStart:(NSString *)start delimiter:(NSString *)delimiter end:(NSString *)end {
    NSMutableString * s = [NSMutableString stringWithString:start];
    __block BOOL first = YES;
    [self forEach:^(id x) {
        if(first) first = NO;
        else [s appendString:delimiter];

        [s appendFormat:@"%@", x];
    }];
    [s appendString:end];
    return s;
}

- (NSString *)charsToString {
    NSMutableString * s = [NSMutableString string];
    [self forEach:^(id x) {
        [s appendFormat:@"%C", [x unsignedShortValue] ];
    }];
    return s;
}

- (BOOL)existsWhere:(BOOL (^)(id))f {
    __block BOOL ret = NO;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:nil yield:^CNGoR(id item) {
        if(f(item)) {
            ret = YES;
            return CNGo_Break;
        }
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}

- (BOOL)allConfirm:(BOOL (^)(id))f {
    __block BOOL ret = YES;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:nil yield:^CNGoR(id item) {
        if(!f(item)) {
            ret = NO;
            return CNGo_Break;
        }
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}

- (CNImTreeSet *)toTreeSet {
    return [self convertWithBuilder:[CNTreeSetBuilder apply]];
}

- (CNChain *)topNumbers:(NSUInteger)numbers {
    return [self link:[CNTopLink topLinkWithNumber:numbers]];
}

- (CNChain *)flat {
    return [self link:[CNFlatLink flatLinkWithFactor:2]];
}

- (CNFuture *)futureF:(id (^)(CNChain *))f {
    CNFutureEnd *lnk = [CNFutureEnd futureEnd];
    [self apply:[lnk yield]];
    return [[lnk future] mapF:^id(id<CNIterable> o) {
        return f([o chain]);
    }];
}

- (CNFuture *)voidFuture {
    CNFutureVoidEnd *lnk = [CNFutureVoidEnd futureVoidEnd];
    [self apply:[lnk yield]];
    return [lnk future];
}

- (CNFuture *)future {
    CNFutureEnd *lnk = [CNFutureEnd futureEnd];
    [self apply:[lnk yield]];
    return [lnk future];
}



- (BOOL)or {
    __block BOOL ret = NO;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:nil yield:^CNGoR(id item) {
        if(unumb(item)) {
            ret = YES;
            return CNGo_Break;
        }
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}

- (BOOL)and {
    __block BOOL ret = YES;
    CNYield *yield = [CNYield alloc];
    yield = [yield initWithBegin:nil yield:^CNGoR(id item) {
        if(!unumb(item)) {
            ret = NO;
            return CNGo_Break;
        }
        return CNGo_Continue;
    } end:nil all:nil];
    [self apply:yield];
    return ret;
}

- (CNChain *)reverseWhen:(BOOL)when {
    if(when) return [self reverse];
    return self;
}

- (CNChain *)shuffle {
    return [self link:[CNShuffleLink shuffleLink]];
}

- (CNImList *)toList {
    return [self convertWithBuilder:[CNImListBuilder imListBuilder]];
}

- (CNChain *)mapOpt:(id(^)(id))f {
    return [self link:[CNMapOptLink mapOptLinkWithF:f]];
}
@end

id cnResolveCollection(id collection) {
    if([collection isKindOfClass:[CNChain class]]) return [collection toArray];
    return collection;
}

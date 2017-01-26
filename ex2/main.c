#include <pthread.h>
#include <stdio.h>

int i = 0;
pthread_mutex_t mutex;

int add1(int value) {
	return value + 1;
}

int sub1(int value) {
	return value - 1;
}

void thread_function(int (*functor)(int)) {
	for (int j = 0; j < 1000000; ++j) {
		pthread_mutex_lock(&mutex);
		i = functor(i);
		pthread_mutex_unlock(&mutex);
	}
}

int main() {
	pthread_t thread_1, thread_2;
	pthread_create(&thread_1, NULL, (void *(*)(void *)) thread_function, add1);
	pthread_create(&thread_2, NULL, (void *(*)(void *)) thread_function, sub1);
	pthread_join(thread_1, NULL);
	pthread_join(thread_2, NULL);
	printf("%d\n", i);
}
